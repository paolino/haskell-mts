module MPF.Backend.RocksDBSpec
    ( spec
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (traverse_)
import Data.List (nub)
import Database.KV.Transaction
    ( RunTransaction (..)
    , newRunTransaction
    )
import Database.KV.Transaction qualified as Transaction
import Database.RocksDB (BatchOp, ColumnFamily)
import MPF.Backend.RocksDB
    ( RunMPFRocksDB (..)
    , withMPFRocksDB
    )
import MPF.Backend.RocksDB qualified as RocksDB
import MPF.Backend.Standalone
    ( MPFStandalone (..)
    , MPFStandaloneCodecs (..)
    )
import MPF.Deletion (deleting)
import MPF.Hashes
    ( MPFHash
    , fromHexKVHashes
    , isoMPFHash
    , mpfHashing
    )
import MPF.Insertion (inserting)
import MPF.Proof.Insertion
    ( mkMPFInclusionProof
    , verifyMPFInclusionProof
    )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Test.QuickCheck
    ( Gen
    , Property
    , Testable (property)
    , elements
    , forAll
    , listOf
    , listOf1
    )

type T a =
    Transaction.Transaction
        IO
        ColumnFamily
        (MPFStandalone ByteString ByteString MPFHash)
        BatchOp
        a

type RunT =
    RunTransaction
        IO
        ColumnFamily
        (MPFStandalone ByteString ByteString MPFHash)
        BatchOp

tempDB :: (RunT -> IO a) -> IO a
tempDB action = withSystemTempDirectory "rocksdb-mpf-test"
    $ \dir -> do
        let path = dir </> "testdb"
        withMPFRocksDB path 1 1 $ \(RunMPFRocksDB run) -> do
            database <-
                run $ RocksDB.mpfStandaloneRocksDBDatabase rocksDBCodecs
            newRunTransaction database >>= action

rocksDBCodecs :: MPFStandaloneCodecs ByteString ByteString MPFHash
rocksDBCodecs =
    MPFStandaloneCodecs
        { mpfKeyCodec = id
        , mpfValueCodec = id
        , mpfNodeCodec = isoMPFHash
        }

iM :: ByteString -> ByteString -> T ()
iM =
    inserting
        []
        fromHexKVHashes
        mpfHashing
        MPFStandaloneKVCol
        MPFStandaloneMPFCol

dM :: ByteString -> T ()
dM =
    deleting
        []
        fromHexKVHashes
        mpfHashing
        MPFStandaloneKVCol
        MPFStandaloneMPFCol

vpfM :: ByteString -> ByteString -> T Bool
vpfM k expectedV = do
    mProof <-
        mkMPFInclusionProof
            []
            fromHexKVHashes
            mpfHashing
            MPFStandaloneMPFCol
            k
    case mProof of
        Nothing -> pure False
        Just proof ->
            verifyMPFInclusionProof
                []
                fromHexKVHashes
                MPFStandaloneMPFCol
                mpfHashing
                expectedV
                proof

testRandomFactsInASparseTree
    :: RunT
    -> Property
testRandomFactsInASparseTree (RunTransaction run) =
    forAll (elements [128 .. 256])
        $ \n -> forAll (genSomePaths n)
            $ \keys -> forAll (listOf $ elements [0 .. length keys - 1])
                $ \ks -> forM_ ks
                    $ \m -> do
                        let kvs =
                                zip keys
                                    $ BC.pack . show <$> [1 :: Int ..]
                            (testKey, testValue) = kvs !! m
                        run $ do
                            traverse_ (uncurry iM) kvs
                        r <- run (vpfM testKey testValue)
                        r `shouldBe` True

genSomePaths :: Int -> Gen [ByteString]
genSomePaths n = fmap nub <$> listOf1 $ do
    let go 0 = return []
        go c = do
            d <- elements [0 .. 255]
            ds <- go (c - 1)
            return (d : ds)
    B.pack <$> go (n `div` 8)

spec :: Spec
spec = around tempDB $ do
    describe "RocksDB MPF backend" $ do
        it "can initialize and close a db"
            $ \_run -> pure @IO ()
        it "verifies a fact" $ \(RunTransaction run) -> run $ do
            iM "key1" "value1"
            r <- vpfM "key1" "value1"
            liftIO $ r `shouldBe` True
        it "rejects an incorrect fact" $ \(RunTransaction run) -> run $ do
            iM "key2" "value2"
            r <- vpfM "key2" "wrongvalue"
            liftIO $ r `shouldBe` False
        it "rejects a deleted fact" $ \(RunTransaction run) -> run $ do
            iM "key3" "value3"
            dM "key3"
            r <- vpfM "key3" "value3"
            liftIO $ r `shouldBe` False
        it "verifies random facts in a sparse tree"
            $ property . testRandomFactsInASparseTree
