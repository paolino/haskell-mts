{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Insertion
-- Description : Insertion algorithm for Compact Sparse Merkle Trees
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module implements the core insertion algorithm for CSMTs.
--
-- The insertion process:
--
-- 1. Build a 'Compose' tree representing the structural changes needed
-- 2. Scan the compose tree to compute hashes and generate database operations
-- 3. Apply all operations atomically
--
-- The 'Compose' type represents a pending tree modification before hashes
-- are computed, allowing for efficient batch processing of changes.
--
-- == Batch Operations
--
-- For bulk loading, use 'insertingBatch' which uses divide-and-conquer
-- to achieve O(n log n) complexity vs O(n²) for sequential inserts.
--
-- For very large datasets, use 'insertingStream' which groups by first
-- direction bit to reduce peak memory by ~2x.
module CSMT.Insertion
    ( -- * Single insertion
      inserting
    , insertingTreeOnly

      -- * Batch insertion
    , insertingBatch
    , insertingStream
    , insertingChunked

      -- * Internal (for testing)
    , buildComposeTree
    , buildComposeFromList
    , scanCompose
    , Compose (..)
    )
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import CSMT.Interface
    ( Direction (..)
    , FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , compareKeys
    , oppositeDirection
    )
import Control.Lens (view)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , insert
    , query
    )

-- |
-- A binary tree structure representing pending modifications.
--
-- * 'Compose' - An internal node with a jump key and two children
-- * 'Leaf' - A leaf node containing an indirect reference
--
-- This structure captures the shape of changes before hashes are computed,
-- allowing the insertion algorithm to build up the tree structure first
-- and then compute all hashes in a single pass.
data Compose a
    = -- | Internal node with jump path and left/right children
      Compose Key (Compose a) (Compose a)
    | -- | Leaf node with indirect value
      Leaf (Indirect a)
    deriving (Show, Eq)

-- | Construct a Compose node with children ordered by direction.
compose :: Direction -> Key -> Compose a -> Compose a -> Compose a
compose L j left right = Compose j left right
compose R j left right = Compose j right left

-- |
-- Insert a key-value pair into the CSMT.
--
-- This function:
--
-- 1. Stores the original key-value pair in the KV store
-- 2. Builds a Compose tree representing the structural changes
-- 3. Computes hashes and applies all CSMT updates atomically
inserting
    :: (Monad m, Ord k, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> k
    -> v
    -> Transaction m cf d ops ()
inserting pfx FromKV{isoK, fromV, treePrefix} hashing kVCol csmtCol k v = do
    insert kVCol k v
    let treeKey = treePrefix v <> view isoK k
    c <- buildComposeTree csmtCol pfx treeKey (fromV v)
    mapM_ (uncurry $ insert csmtCol) $ snd $ scanCompose pfx hashing c

-- | Insert into the tree column only (no KV write).
-- Used during journal replay when KV is already up to date.
insertingTreeOnly
    :: (Monad m, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d Key (Indirect a)
    -> k
    -> v
    -> Transaction m cf d ops ()
insertingTreeOnly pfx FromKV{isoK, fromV, treePrefix} hashing csmtCol k v = do
    let treeKey = treePrefix v <> view isoK k
    c <- buildComposeTree csmtCol pfx treeKey (fromV v)
    mapM_ (uncurry $ insert csmtCol) $ snd $ scanCompose pfx hashing c

-- |
-- Scan a Compose tree bottom-up, computing hashes and collecting database operations.
--
-- Returns the root indirect value and a list of (key, value) pairs to insert.
-- Hashes are computed by combining child hashes at each internal node.
scanCompose
    :: Key
    -- ^ Prefix (use @[]@ for root)
    -> Hashing a
    -> Compose a
    -> (Indirect a, [(Key, Indirect a)])
scanCompose pfx Hashing{combineHash} = go pfx
  where
    go k (Leaf i) = (i, [(k, i)])
    go k (Compose jump left right) =
        let k' = k <> jump
            (hl, ls) = go (k' <> [L]) left
            (hr, rs) = go (k' <> [R]) right
            value = combineHash hl hr
            i = Indirect{jump, value}
        in  (i, ls <> rs <> [(k, i)])

-- |
-- Build a Compose tree for inserting a value at the given key.
--
-- Traverses the existing tree structure to find where the new value
-- should be inserted, handling:
--
-- * Empty slots - create a new leaf
-- * Existing values - split nodes as needed
-- * Path compression - maintain compact representation
buildComposeTree
    :: forall a d ops cf m
     . (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -- ^ Prefix (use @[]@ for root)
    -> Key
    -> a
    -> Transaction m cf d ops (Compose a)
buildComposeTree csmtCol pfx key h = go key pfx pure
  where
    go [] _ cont = cont $ Leaf $ Indirect [] h
    go target current cont = do
        mi <- query csmtCol current
        case mi of
            Nothing -> cont $ Leaf $ Indirect target h
            Just Indirect{jump, value} -> do
                let (common, other, us) = compareKeys jump target
                case (other, us) of
                    ([], []) -> cont $ Leaf $ Indirect common h
                    ([], z : zs) -> do
                        mov <- query csmtCol (current <> common <> [oppositeDirection z])
                        case mov of
                            Nothing -> error "a jump pointed to a non-existing node"
                            Just i ->
                                go zs (current <> common <> [z]) $ \c ->
                                    cont $ compose z common c $ Leaf i
                    (_ : os, z : zs) ->
                        go zs (current <> common <> [z]) $ \c ->
                            cont $ compose z common c $ Leaf $ Indirect{jump = os, value}
                    _ ->
                        error
                            "there is at least on key longer than the requested key to insert"

-- |
-- Batch insert multiple key-value pairs into an empty CSMT.
--
-- This is much faster than sequential inserts as it builds the tree in one pass
-- using divide-and-conquer: O(n log n) vs O(n²).
--
-- Note: This function assumes the tree is empty. For inserting into an existing
-- tree, use 'inserting' or 'insertingChunked'.
insertingBatch
    :: (Monad m, Ord k, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> [(k, v)]
    -> Transaction m cf d ops ()
insertingBatch pfx FromKV{isoK, fromV, treePrefix} hashing kvCol csmtCol kvs = do
    -- Insert all key-value pairs into the KV store
    mapM_ (uncurry $ insert kvCol) kvs
    -- Build the CSMT tree in one pass and insert all nodes
    let keyVals = [(treePrefix v <> view isoK k, fromV v) | (k, v) <- kvs]
        mCompose = buildComposeFromList keyVals
    case mCompose of
        Nothing -> pure () -- Empty list
        Just c -> mapM_ (uncurry $ insert csmtCol) $ snd $ scanCompose pfx hashing c

-- |
-- Build a Compose tree from a list of key-value pairs.
--
-- Uses divide-and-conquer: find common prefix, split by first differing bit, recurse.
buildComposeFromList :: [(Key, a)] -> Maybe (Compose a)
buildComposeFromList [] = Nothing
buildComposeFromList [(key, value)] =
    -- Single element: create a leaf
    Just $ Leaf $ Indirect key value
buildComposeFromList kvs =
    -- Multiple elements: find common prefix, then branch
    let prefix = commonPrefixAll (map fst kvs)
        prefixLen = length prefix
        -- Strip prefix and group by first direction
        stripped = [(drop prefixLen k, v) | (k, v) <- kvs]
        -- Group by first direction
        grouped = groupByFirstDir stripped
        -- Recursively build children
        mLeft = buildComposeFromList (Map.findWithDefault [] L grouped)
        mRight = buildComposeFromList (Map.findWithDefault [] R grouped)
    in  case (mLeft, mRight) of
            (Nothing, Nothing) -> Nothing
            (Just l, Nothing) -> Just $ prependPrefix prefix l
            (Nothing, Just r) -> Just $ prependPrefix prefix r
            (Just l, Just r) -> Just $ Compose prefix l r

-- | Prepend a prefix to a Compose tree
prependPrefix :: Key -> Compose a -> Compose a
prependPrefix [] c = c
prependPrefix p (Leaf (Indirect j v)) = Leaf (Indirect (p <> j) v)
prependPrefix p (Compose j l r) = Compose (p <> j) l r

-- | Find the common prefix of all keys
commonPrefixAll :: [Key] -> Key
commonPrefixAll [] = []
commonPrefixAll [k] = k
commonPrefixAll (k : ks) = foldl' commonPrefix2 k ks

-- | Find common prefix of two keys
commonPrefix2 :: Key -> Key -> Key
commonPrefix2 [] _ = []
commonPrefix2 _ [] = []
commonPrefix2 (x : xs) (y : ys)
    | x == y = x : commonPrefix2 xs ys
    | otherwise = []

-- | Group key-value pairs by their first direction
groupByFirstDir :: [(Key, a)] -> Map Direction [(Key, a)]
groupByFirstDir = foldl' addToGroup Map.empty
  where
    addToGroup acc ([], _) = acc -- Should not happen after stripping common prefix
    addToGroup acc (d : rest, v) =
        Map.insertWith (++) d [(rest, v)] acc

-- |
-- Streaming batch insert for large datasets.
--
-- Groups items by first direction bit and processes each group separately,
-- reducing peak memory by ~2x compared to full batch insert.
-- Still requires O(n) memory for the input list and one group at a time.
insertingStream
    :: forall m k v a d cf ops
     . (Monad m, Ord k, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> [(k, v)]
    -> Transaction m cf d ops ()
insertingStream pfx FromKV{isoK, fromV, treePrefix} hashing kvCol csmtCol kvs = do
    -- Insert all key-value pairs into the KV store first
    mapM_ (uncurry $ insert kvCol) kvs

    -- Convert to keys and group by first direction
    let keyVals = [(treePrefix v <> view isoK k, fromV v) | (k, v) <- kvs]
        grouped = groupByFirstDir keyVals
        leftItems = Map.findWithDefault [] L grouped
        rightItems = Map.findWithDefault [] R grouped

    case (null leftItems, null rightItems) of
        (True, True) -> pure ()
        (False, True) -> do
            -- Only left side: insert directly with full keys (prepend L back)
            let fullKeys = [([L] <> k, v) | (k, v) <- leftItems]
            case buildComposeFromList fullKeys of
                Nothing -> pure ()
                Just tree ->
                    mapM_ (uncurry $ insert csmtCol) $ snd $ scanCompose pfx hashing tree
        (True, False) -> do
            -- Only right side: insert directly with full keys (prepend R back)
            let fullKeys = [([R] <> k, v) | (k, v) <- rightItems]
            case buildComposeFromList fullKeys of
                Nothing -> pure ()
                Just tree ->
                    mapM_ (uncurry $ insert csmtCol) $ snd $ scanCompose pfx hashing tree
        (False, False) -> do
            -- Both sides: process independently and combine at root
            childResults <- mapM processGroup [(L, leftItems), (R, rightItems)]
            let mLeft = lookup L childResults >>= id
                mRight = lookup R childResults >>= id
            case (mLeft, mRight) of
                (Just l, Just r) -> do
                    let rootValue = combineHash hashing l r
                        rootNode = Indirect [] rootValue
                    insert csmtCol pfx rootNode
                _ -> pure () -- Should not happen
  where
    processGroup
        :: (Direction, [(Key, a)])
        -> Transaction m cf d ops (Direction, Maybe (Indirect a))
    processGroup (dir, items) = do
        case buildComposeFromList items of
            Nothing -> pure (dir, Nothing)
            Just tree -> do
                let (rootInd, inserts) = scanCompose (pfx <> [dir]) hashing tree
                -- Write all nodes for this subtree
                mapM_ (uncurry $ insert csmtCol) inserts
                pure (dir, Just rootInd)

-- |
-- Chunked insert for very large datasets (millions of items).
--
-- Processes items in chunks to bound memory usage. Works with any backend.
--
-- For truly large datasets (10M+), use this with the RocksDB backend which
-- persists data to disk between chunks, keeping memory bounded.
--
-- Returns the number of chunks processed.
insertingChunked
    :: (Monad m, Ord k, GCompare d)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> Int
    -- ^ Chunk size (e.g., 50000)
    -> [(k, v)]
    -> Transaction m cf d ops Int
insertingChunked pfx fkv hashing kvCol csmtCol chunkSize kvs = do
    let chunks = chunksOf chunkSize kvs
    go 0 chunks
  where
    go !n [] = pure n
    go !n (chunk : rest) = do
        -- Insert this chunk item by item
        mapM_ (\(k, v) -> inserting pfx fkv hashing kvCol csmtCol k v) chunk
        -- Continue with rest
        go (n + 1) rest

-- | Split a list into chunks of the given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (chunk, rest) = splitAt n xs
    in  chunk : chunksOf n rest
