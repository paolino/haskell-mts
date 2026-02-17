{-# LANGUAGE OverloadedLists #-}

module CSMT.TreePrefixSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , Key
    , Standalone (StandaloneCSMTCol)
    )
import CSMT.Backend.Pure
    ( InMemoryDB
    , emptyInMemoryDB
    , inMemoryCSMTParsed
    , runPure
    , runPureTransaction
    )
import CSMT.Interface (FromKV (..))
import CSMT.Proof.Completeness
    ( collectValues
    )
import CSMT.Test.Lib
    ( deleteM
    , genSomePaths
    , insertM
    , proofM
    , verifyM
    , word64Codecs
    , word64Hashing
    )
import Data.Foldable (foldl')
import Data.Word (Word64)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
    ( forAll
    , property
    , shuffle
    )
import Test.QuickCheck.Gen (elements)

-- | FromKV with a prefix derived from the value.
-- Even values get prefix [L], odd values get prefix [R].
prefixedFromKV :: FromKV Key Word64 Word64
prefixedFromKV =
    FromKV
        { fromK = id
        , fromV = id
        , treePrefix = \v -> if even v then [L] else [R]
        }

insertP :: Key -> Word64 -> InMemoryDB -> InMemoryDB
insertP k v db =
    snd
        $ runPure db
        $ insertM word64Codecs prefixedFromKV word64Hashing k v

deleteP :: Key -> InMemoryDB -> InMemoryDB
deleteP k db =
    snd $ runPure db $ deleteM word64Codecs prefixedFromKV word64Hashing k

spec :: Spec
spec = do
    describe "treePrefix" $ do
        it "prefixes the tree key on insertion" $ do
            -- Insert key [R] with even value 2 → prefix [L]
            -- tree key = [L] <> [R] = [L, R]
            -- CSMT stores this as single leaf at [] with jump [L,R]
            let db = insertP [R] 2 emptyInMemoryDB
                csmt = inMemoryCSMTParsed word64Codecs db
            -- Single leaf: tree key [L,R] stored at path [] with jump [L,R]
            length csmt `shouldBe` 1

        it "different values produce different tree keys for same key" $ do
            -- key [R], even value 2 → tree key [L, R]
            -- key [R], odd value 3  → tree key [R, R]
            -- These are different tree entries despite the same user key
            let db = insertP [R] 2 $ insertP [R] 3 emptyInMemoryDB
                csmt = inMemoryCSMTParsed word64Codecs db
            -- Should have 3 nodes: root + 2 leaves
            length csmt `shouldBe` 3

        it "deletion removes the prefixed tree key" $ do
            let db = deleteP [R] $ insertP [R] 2 emptyInMemoryDB
            db `shouldBe` emptyInMemoryDB

        it "insert and delete round-trip with prefix" $ do
            let db =
                    deleteP [L]
                        $ deleteP [R]
                        $ insertP [L] 4
                        $ insertP [R] 7 emptyInMemoryDB
            db `shouldBe` emptyInMemoryDB

        it "generates valid inclusion proof with prefix" $ do
            let (result, _) = runPure emptyInMemoryDB $ do
                    insertM word64Codecs prefixedFromKV word64Hashing [L] 2
                    insertM word64Codecs prefixedFromKV word64Hashing [R] 5
                    proofM word64Codecs prefixedFromKV word64Hashing [L]
            result `shouldSatisfy` \case
                Nothing -> False
                Just (v, _) -> v == 2

        it "collectValues finds entries by prefix" $ do
            -- Insert entries with both even and odd values to ensure
            -- the tree splits at the prefix level [L] vs [R]:
            --   key [L], value 2 (even) → tree key [L, L]
            --   key [R], value 4 (even) → tree key [L, R]
            --   key [L], value 3 (odd)  → tree key [R, L]
            let db =
                    insertP [L] 2
                        $ insertP [R] 4
                        $ insertP [L] 3 emptyInMemoryDB
                collectedL =
                    fst
                        $ runPure db
                        $ runPureTransaction word64Codecs
                        $ collectValues StandaloneCSMTCol [L]
                collectedR =
                    fst
                        $ runPure db
                        $ runPureTransaction word64Codecs
                        $ collectValues StandaloneCSMTCol [R]
            length collectedL `shouldBe` 2
            length collectedR `shouldBe` 1

    describe "treePrefix properties" $ do
        it "insert then delete all yields empty DB"
            $ property
            $ forAll (elements [1 .. 8])
            $ \n ->
                forAll (genSomePaths n) $ \keys -> do
                    let kvs = zip keys [1 :: Word64 ..]
                        full = foldl' (\db (k, v) -> insertP k v db) emptyInMemoryDB kvs
                        emptied = foldl' (flip deleteP) full (map fst kvs)
                    emptied `shouldBe` emptyInMemoryDB

        it "insert then delete in random order yields empty DB"
            $ property
            $ forAll (elements [1 .. 8])
            $ \n ->
                forAll (genSomePaths n) $ \keys ->
                    forAll (shuffle keys) $ \shuffled -> do
                        let kvs = zip keys [1 :: Word64 ..]
                            full = foldl' (\db (k, v) -> insertP k v db) emptyInMemoryDB kvs
                            emptied = foldl' (flip deleteP) full shuffled
                        emptied `shouldBe` emptyInMemoryDB

        it "all inserted entries have valid inclusion proofs"
            $ property
            $ forAll (elements [1 .. 8])
            $ \n ->
                forAll (genSomePaths n) $ \keys -> do
                    let kvs = zip keys [1 :: Word64 ..]
                        db = foldl' (\d (k, v) -> insertP k v d) emptyInMemoryDB kvs
                        results =
                            map
                                ( \(k, v) ->
                                    fst
                                        $ runPure db
                                        $ verifyM word64Codecs prefixedFromKV word64Hashing k v
                                )
                                kvs
                    results `shouldBe` replicate (length kvs) True

        it "prefix changes tree structure vs no prefix"
            $ property
            $ forAll (elements [2 .. 6])
            $ \n ->
                forAll (genSomePaths n) $ \keys -> do
                    let kvs = zip keys [2, 4 :: Word64 ..]
                        noPrefixFromKV =
                            FromKV
                                { fromK = id
                                , fromV = id
                                , treePrefix = const []
                                }
                        dbPrefixed =
                            foldl'
                                (\d (k, v) -> insertP k v d)
                                emptyInMemoryDB
                                kvs
                        dbPlain =
                            foldl'
                                ( \d (k, v) ->
                                    snd
                                        $ runPure d
                                        $ insertM word64Codecs noPrefixFromKV word64Hashing k v
                                )
                                emptyInMemoryDB
                                kvs
                        csmtPrefixed = inMemoryCSMTParsed word64Codecs dbPrefixed
                        csmtPlain = inMemoryCSMTParsed word64Codecs dbPlain
                    -- With prefix, tree structure differs from without
                    csmtPrefixed `shouldSatisfy` (/= csmtPlain)
