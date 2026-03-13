{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CSMT Benchmark - Batch vs Sequential Insertion
module Main where

import CSMT (Direction (..), Indirect (..), Key)
import CSMT.Backend.Pure (InMemoryDB, Pure, emptyInMemoryDB, runPure)
import CSMT.Hashes (Hash, hashHashing, mkHash, renderHash)
import CSMT.Test.Lib
    ( hashCodecs
    , identityFromKV
    , insertBatchM
    , insertM
    , insertStreamM
    , proofM
    , verifyM
    )
import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- | Convert a ByteString to a Key (list of directions based on bits)
byteStringToKey :: ByteString -> Key
byteStringToKey bs = concatMap byteToDirs (B8.unpack bs)
  where
    byteToDirs c =
        let n = fromEnum c
        in  [if testBit n i then R else L | i <- [7, 6 .. 0]]
    testBit x i = (x `div` (2 ^ i)) `mod` 2 == 1

-- | Generate deterministic test data
generateTestData :: Int -> [(Key, Hash)]
generateTestData count =
    [ ( byteStringToKey $ B8.pack $ "key-" <> padLeft 8 '0' (show i)
      , mkHash $ B8.pack $ "value-" <> show i
      )
    | i <- [0 .. count - 1]
    ]
  where
    padLeft n c s = replicate (n - length s) c <> s

-- | Benchmark helper
benchmark :: String -> IO a -> IO (a, Double)
benchmark name action = do
    putStr $ name ++ "... "
    hFlush stdout
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let durationMs = realToFrac (diffUTCTime end start) * 1000 :: Double
    printf "%.2fms\n" durationMs
    pure (result, durationMs)

-- | Run all insertions sequentially
insertAllSequential :: [(Key, Hash)] -> (Maybe Hash, InMemoryDB)
insertAllSequential testData =
    let action :: Pure (Maybe Hash)
        action = do
            mapM_
                (uncurry $ insertM hashCodecs identityFromKV hashHashing)
                testData
            mRoot <- getRootHash
            pure mRoot
    in  runPure emptyInMemoryDB action

-- | Run all insertions using batch insert
insertAllBatch :: [(Key, Hash)] -> (Maybe Hash, InMemoryDB)
insertAllBatch testData =
    let action :: Pure (Maybe Hash)
        action = do
            insertBatchM hashCodecs identityFromKV hashHashing testData
            mRoot <- getRootHash
            pure mRoot
    in  runPure emptyInMemoryDB action

-- | Run all insertions using streaming insert
insertAllStream :: [(Key, Hash)] -> (Maybe Hash, InMemoryDB)
insertAllStream testData =
    let action :: Pure (Maybe Hash)
        action = do
            insertStreamM hashCodecs identityFromKV hashHashing testData
            mRoot <- getRootHash
            pure mRoot
    in  runPure emptyInMemoryDB action

-- | Get root hash from database
getRootHash :: Pure (Maybe Hash)
getRootHash = do
    mp <- proofM hashCodecs identityFromKV hashHashing []
    pure $ case mp of
        Nothing -> Nothing
        Just (_, proof) -> Nothing -- We'd need to extract from proof

-- | Generate proofs for sample keys
generateProofs :: [(Key, Hash)] -> InMemoryDB -> Int
generateProofs testData db =
    let action :: Pure Int
        action = do
            results <- forM (take 100 testData) $ \(k, _) -> do
                mProof <- proofM hashCodecs identityFromKV hashHashing k
                pure $ maybe 0 (const 1) mProof
            pure $ sum results
    in  fst $ runPure db action

-- | Verify sample key-value pairs
verifyAll :: [(Key, Hash)] -> InMemoryDB -> Int
verifyAll testData db =
    let action :: Pure Int
        action = do
            results <- forM (take 100 testData) $ \(k, v) -> do
                verified <- verifyM hashCodecs identityFromKV hashHashing k v
                pure $ if verified then 1 else 0
            pure $ sum results
    in  fst $ runPure db action

data InsertMethod = Sequential | Batch | Stream
    deriving (Eq)

methodName :: InsertMethod -> String
methodName Sequential = "sequential"
methodName Batch = "batch"
methodName Stream = "stream"

-- | Run benchmark for a given count
runBenchmark :: InsertMethod -> Bool -> Int -> IO ()
runBenchmark method skipProofs count = do
    putStrLn
        $ "\n=== CSMT Benchmark (n="
            ++ show count
            ++ ", method="
            ++ methodName method
            ++ ") ===\n"

    -- Generate test data
    putStr $ "Generating " ++ show count ++ " test items... "
    hFlush stdout
    start <- getCurrentTime
    let testData = generateTestData count
    _ <- pure $! length testData
    end <- getCurrentTime
    printf "%.2fs\n" (realToFrac (diffUTCTime end start) :: Double)

    -- Benchmark: Insert all items
    (_, db, insertTime) <- case method of
        Sequential -> do
            ((mRoot, db'), t) <- benchmark ("Insert " ++ show count ++ " items (sequential)") $ do
                pure $! insertAllSequential testData
            pure (mRoot, db', t)
        Batch -> do
            ((mRoot, db'), t) <- benchmark ("Insert " ++ show count ++ " items (batch)") $ do
                pure $! insertAllBatch testData
            pure (mRoot, db', t)
        Stream -> do
            ((mRoot, db'), t) <- benchmark ("Insert " ++ show count ++ " items (stream)") $ do
                pure $! insertAllStream testData
            pure (mRoot, db', t)

    -- For very large datasets, skip proof generation/verification
    when (not skipProofs && count <= 10000) $ do
        (proofsGenerated, proofGenTime) <- benchmark ("Generate 100 proofs") $ do
            pure $! generateProofs testData db

        printf "Proofs generated: %d/100\n" proofsGenerated

        (verified, verifyTime) <- benchmark ("Verify 100 proofs") $ do
            pure $! verifyAll testData db

        printf "Verified: %d/100\n" verified

        printf
            "Proof gen rate: %.0f ops/sec\n"
            (100 / proofGenTime * 1000 :: Double)
        printf
            "Verify rate: %.0f ops/sec\n"
            (100 / verifyTime * 1000 :: Double)

    -- Summary
    putStrLn "\n--- Summary ---"
    printf
        "Insert rate: %.0f ops/sec\n"
        (fromIntegral count / insertTime * 1000 :: Double)
    printf "Total insert time: %.2fs\n" (insertTime / 1000)

main :: IO ()
main = do
    args <- getArgs
    let (method, skipProofs, counts) = parseArgs args

    putStrLn "CSMT Benchmark - Batch vs Sequential"
    putStrLn "====================================="
    putStrLn $ "(Using " ++ methodName method ++ " insert)"
    when skipProofs $ putStrLn "(Skipping proof generation/verification)"

    mapM_ (runBenchmark method skipProofs) counts

parseArgs :: [String] -> (InsertMethod, Bool, [Int])
parseArgs args =
    let useSequential = "--sequential" `elem` args
        useStream = "--stream" `elem` args
        skipProofs = "--skip-proofs" `elem` args
        method
            | useSequential = Sequential
            | useStream = Stream
            | otherwise = Batch
        nums = [read x | x <- args, all (`elem` ['0' .. '9']) x, not (null x)]
        counts = if null nums then [100, 1000, 10000] else nums
    in  (method, skipProofs, counts)
