{-# LANGUAGE OverloadedStrings #-}

module MPF.Hashes.CBORSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import MPF.Hashes
    ( MPFHash
    , mkMPFHash
    , renderMPFHash
    )
import MPF.Hashes.CBOR (parseProof, renderProof)
import MPF.Interface (byteStringToHexKey)
import MPF.Proof.Insertion (MPFProof (..))
import MPF.Test.Lib
    ( fruitsTestData
    , insertByteStringM
    , proofMPFM
    , runMPFPure'
    )
import Test.Hspec

-- | Generate a proof for a given fruit key against the full 30-fruit trie
fruitProof :: ByteString -> Maybe (MPFProof MPFHash)
fruitProof fruitKey =
    fst $ runMPFPure' $ do
        forM_ fruitsTestData $ uncurry insertByteStringM
        let hexKey =
                byteStringToHexKey
                    $ renderMPFHash
                    $ mkMPFHash fruitKey
        proofMPFM hexKey

spec :: Spec
spec = describe "MPF.Hashes.CBOR" $ do
    describe "round-trip" $ do
        it "mango proof survives render/parse" $ do
            case fruitProof "mango[uid: 0]" of
                Nothing -> expectationFailure "Failed to generate mango proof"
                Just proof -> do
                    let bs = renderProof proof
                    parseProof bs `shouldBe` Just proof

        it "kumquat proof survives render/parse" $ do
            case fruitProof "kumquat[uid: 0]" of
                Nothing -> expectationFailure "Failed to generate kumquat proof"
                Just proof -> do
                    let bs = renderProof proof
                    parseProof bs `shouldBe` Just proof

        it "apple proof survives render/parse" $ do
            case fruitProof "apple[uid: 58]" of
                Nothing -> expectationFailure "Failed to generate apple proof"
                Just proof -> do
                    let bs = renderProof proof
                    parseProof bs `shouldBe` Just proof

    describe "invalid input" $ do
        it "returns Nothing for empty input" $ do
            parseProof "" `shouldBe` Nothing

        it "returns Nothing for garbage input" $ do
            parseProof "not-cbor-data" `shouldBe` Nothing
