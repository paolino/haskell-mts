{-# LANGUAGE OverloadedStrings #-}

module MPF.Hashes.AikenSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import MPF.Hashes
    ( MPFHash
    , mkMPFHash
    , renderMPFHash
    )
import MPF.Hashes.Aiken (renderAikenProof)
import MPF.Interface (byteStringToHexKey)
import MPF.Proof.Insertion (MPFProof (..))
import MPF.Test.Lib
    ( encodeHex
    , fruitsTestData
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

-- | Expected Aiken CBOR hex for mango proof
-- From aiken-lang/merkle-patricia-forestry off-chain/tests/trie.test.js
mangoExpectedHex :: ByteString
mangoExpectedHex =
    "9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f58401508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4dffffd87b9f00582009d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc5820c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795ffff"

-- | Expected Aiken CBOR hex for kumquat proof
kumquatExpectedHex :: ByteString
kumquatExpectedHex =
    "9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c58403490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15bffffd87a9f00d8799f0041075820a1ffbc0e72342b41129e2d01d289809079b002e54b123860077d2d66added281ffffff"

spec :: Spec
spec = describe "MPF.Hashes.Aiken" $ do
    describe "Aiken test vectors" $ do
        it "mango proof matches Aiken CBOR" $ do
            case fruitProof "mango[uid: 0]" of
                Nothing -> expectationFailure "Failed to generate mango proof"
                Just proof -> do
                    let cbor = renderAikenProof (mpfProofSteps proof)
                    encodeHex cbor `shouldBe` mangoExpectedHex

        it "kumquat proof matches Aiken CBOR" $ do
            case fruitProof "kumquat[uid: 0]" of
                Nothing -> expectationFailure "Failed to generate kumquat proof"
                Just proof -> do
                    let cbor = renderAikenProof (mpfProofSteps proof)
                    encodeHex cbor `shouldBe` kumquatExpectedHex
