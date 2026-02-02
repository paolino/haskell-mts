{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | MPF Benchmark - Haskell Implementation
-- Compares against the TypeScript/Aiken implementation
module Main where

import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import MPF.Hashes (mkMPFHash, renderMPFHash)
import MPF.Interface (byteStringToHexKey)
import MPF.Test.Lib
    ( MPFInMemoryDB
    , MPFPure
    , emptyMPFInMemoryDB
    , getRootHashM
    , insertBatchMPFM
    , insertBulkMPFM
    , proofMPFM
    , runMPFPure
    , verifyMPFM
    )
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- | Generate deterministic test data
generateTestData :: Int -> [(ByteString, ByteString)]
generateTestData count =
    [ (B8.pack $ "key-" <> padLeft 6 '0' (show i), B8.pack $ "value-" <> show i)
    | i <- [0 .. count - 1]
    ]
  where
    padLeft n c s = replicate (n - length s) c <> s

-- | Benchmark helper
benchmark :: String -> IO a -> IO (a, Double)
benchmark name action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let durationMs = realToFrac (diffUTCTime end start) * 1000 :: Double
    printf "%s: %.2fms\n" name durationMs
    pure (result, durationMs)

-- | Run all insertions using batch insert (for small/medium datasets)
-- Uses divide-and-conquer for O(n log n) performance
insertAllBatch :: [(ByteString, ByteString)] -> (Maybe ByteString, MPFInMemoryDB)
insertAllBatch testData =
    let action :: MPFPure (Maybe ByteString)
        action = do
            let kvs = [(byteStringToHexKey k, mkMPFHash v) | (k, v) <- testData]
            insertBatchMPFM kvs
            mRoot <- getRootHashM
            pure $ renderMPFHash <$> mRoot
    in  runMPFPure emptyMPFInMemoryDB action

-- | Run all insertions using bulk insert (for large datasets)
-- Sorts by key and inserts sequentially - better for millions of items
insertAllBulk :: [(ByteString, ByteString)] -> (Maybe ByteString, MPFInMemoryDB)
insertAllBulk testData =
    let action :: MPFPure (Maybe ByteString)
        action = do
            let kvs = [(byteStringToHexKey k, mkMPFHash v) | (k, v) <- testData]
            insertBulkMPFM kvs
            mRoot <- getRootHashM
            pure $ renderMPFHash <$> mRoot
    in  runMPFPure emptyMPFInMemoryDB action

-- | Generate proofs for all keys using an existing database
generateProofs :: [(ByteString, ByteString)] -> MPFInMemoryDB -> Int
generateProofs testData db =
    let action :: MPFPure Int
        action = do
            results <- forM testData $ \(k, _) -> do
                let key = byteStringToHexKey k
                mProof <- proofMPFM key
                pure $ maybe 0 (const 1) mProof
            pure $ sum results
    in  fst $ runMPFPure db action

-- | Verify all key-value pairs using an existing database
verifyAll :: [(ByteString, ByteString)] -> MPFInMemoryDB -> Int
verifyAll testData db =
    let action :: MPFPure Int
        action = do
            results <- forM testData $ \(k, v) -> do
                let key = byteStringToHexKey k
                    value = mkMPFHash v
                verified <- verifyMPFM key value
                pure $ if verified then 1 else 0
            pure $ sum results
    in  fst $ runMPFPure db action

-- | Run benchmark for a given count
runBenchmark :: Bool -> Bool -> Int -> IO ()
runBenchmark useBulk skipProofs count = do
    let method = if useBulk then "bulk" else "batch" :: String
    putStrLn $ "\n=== Haskell MPF Benchmark (n=" ++ show count ++ ", method=" ++ method ++ ") ===\n"

    -- Generate test data
    putStr $ "Generating " ++ show count ++ " test items... "
    hFlush stdout
    start <- getCurrentTime
    let testData = generateTestData count
    -- Force evaluation by computing length
    _ <- pure $! length testData
    end <- getCurrentTime
    printf "%.2fs\n" (realToFrac (diffUTCTime end start) :: Double)

    -- Benchmark: Insert all items
    let insertFn = if useBulk then insertAllBulk else insertAllBatch
    ((mRootHash, db), insertTime) <- benchmark ("Insert " ++ show count ++ " items (" ++ method ++ ")") $ do
        let !result = insertFn testData
        pure result

    case mRootHash of
        Just h -> printf "Root hash: %s\n" (B8.unpack h)
        Nothing -> putStrLn "Root hash: (empty)"

    -- For very large datasets, skip proof generation/verification
    when (not skipProofs) $ do
        -- Benchmark: Generate proofs for all items
        (proofsGenerated, proofGenTime) <- benchmark (printf "Generate %d proofs" count) $ do
            pure $! generateProofs testData db

        printf "Proofs generated: %d/%d\n" proofsGenerated count

        -- Benchmark: Verify all proofs
        (verified, verifyTime) <- benchmark (printf "Verify %d proofs" count) $ do
            pure $! verifyAll testData db

        printf "Verified: %d/%d\n" verified count

        printf "Proof gen rate: %.0f ops/sec\n" (fromIntegral count / proofGenTime * 1000 :: Double)
        printf "Verify rate: %.0f ops/sec\n" (fromIntegral count / verifyTime * 1000 :: Double)

    -- Summary
    putStrLn "\n--- Summary ---"
    printf "Insert rate: %.0f ops/sec\n" (fromIntegral count / insertTime * 1000 :: Double)
    printf "Total insert time: %.2fs\n" (insertTime / 1000)

main :: IO ()
main = do
    args <- getArgs
    let (useBulk, skipProofs, counts) = parseArgs args

    putStrLn "MPF Benchmark - Haskell Implementation"
    putStrLn "======================================"
    when useBulk $ putStrLn "(Using bulk insert - sorted sequential)"
    when skipProofs $ putStrLn "(Skipping proof generation/verification)"

    mapM_ (runBenchmark useBulk skipProofs) counts

parseArgs :: [String] -> (Bool, Bool, [Int])
parseArgs args =
    let useBulk = "--bulk" `elem` args
        skipProofs = "--skip-proofs" `elem` args
        nums = [read x | x <- args, all (`elem` ['0'..'9']) x, not (null x)]
        counts = if null nums then [100, 1000] else nums
    in (useBulk, skipProofs, counts)
