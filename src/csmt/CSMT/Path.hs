-- |
-- Module      : CSMT.Path
-- Description : Unified path extraction for Compact Sparse Merkle Trees
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides a unified abstraction for extracting paths from CSMTs.
-- Both deletion and proof generation traverse an existing path from root to a
-- target key, collecting sibling information. This module factors out that
-- common pattern.
--
-- == Overview
--
-- When performing operations on a CSMT, we often need to traverse from the root
-- to a specific key, recording information about each node along the way:
--
-- * __Deletion__: We need siblings to update parent hashes after removing a node
-- * __Proof generation__: We need siblings to construct Merkle inclusion proofs
--
-- The 'TreePath' type captures this traversal structure, which can then be
-- interpreted for different purposes:
--
-- * Convert to database operations via 'pathToOps' (for deletion)
-- * Convert to proof steps via 'pathToProofSteps' (for proof generation)
--
-- == Example
--
-- Given a tree with keys @[L, L]@ and @[L, R]@:
--
-- @
-- mPath <- extractPath csmtSel [L, L]
-- case mPath of
--     Nothing -> -- key not found
--     Just path -> do
--         let ops = pathToOps hashing path  -- for deletion
--         let (rootJump, leafVal, steps) = pathToProofSteps path  -- for proofs
-- @
module CSMT.Path
    ( -- * Path Type
      TreePath (..)

      -- * Path Extraction
    , extractPath

      -- * Path Interpretation
    , pathToOps
    , pathToProofSteps
    )
where

import CSMT.Interface
    ( Direction (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , addWithDirection
    , compareKeys
    , oppositeDirection
    )
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )

-- |
-- A path through the CSMT from root to a target key.
--
-- This recursive structure captures the entire traversal from root to leaf,
-- including all sibling references needed for operations that modify or
-- verify the tree.
--
-- == Structure
--
-- * 'PathLeaf' - Terminal node containing the target value
-- * 'PathBranch' - Internal node recording the direction taken and sibling
--
-- == Invariants
--
-- * The path always leads to an existing key in the tree
-- * Each 'PathBranch' records the sibling at that branch point
-- * Jump paths are stored to support path compression
data TreePath a
    = -- | A leaf node reached at the end of the path.
      --
      -- * First argument: jump path prefix at this node
      -- * Second argument: the value stored at this leaf
      PathLeaf Key a
    | -- | A branch node along the path to the target.
      --
      -- * First argument: jump path prefix at this node
      -- * Second argument: direction taken toward target ('L' or 'R')
      -- * Third argument: the child subtree (rest of path)
      -- * Fourth argument: the sibling's indirect reference (for hash updates)
      PathBranch Key Direction (TreePath a) (Indirect a)
    deriving (Show, Eq)

-- |
-- Extract a path for the given key from the CSMT.
--
-- Traverses the tree from root to the target key, recording each branch
-- and its sibling along the way.
--
-- == Return value
--
-- * @'Just' path@ - The key exists and @path@ captures the full traversal
-- * 'Nothing' - The key does not exist in the tree (path diverges or is missing)
--
-- == Algorithm
--
-- 1. Start at the root node
-- 2. At each node, verify the jump path matches the remaining key
-- 3. If key is exhausted, we've reached the leaf ('PathLeaf')
-- 4. Otherwise, follow the next direction bit, record the sibling, and recurse
extractPath
    :: forall a d ops cf m
     . (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -- ^ Selector for the CSMT column
    -> Key
    -- ^ The key to find a path for
    -> Transaction m cf d ops (Maybe (TreePath a))
extractPath csmtSel = runMaybeT . go []
  where
    go
        :: Key
        -> Key
        -> MaybeT (Transaction m cf d ops) (TreePath a)
    go current remaining = do
        Indirect{jump = j, value = v} <- MaybeT $ query csmtSel current
        let (_common, other, remaining') = compareKeys j remaining
        guard $ null other
        case remaining' of
            [] -> pure $ PathLeaf j v
            (r : remaining'') -> do
                let current' = current <> j
                sibling <-
                    MaybeT $ query csmtSel (current' <> [oppositeDirection r])
                p <- go (current' <> [r]) remaining''
                pure $ PathBranch j r p sibling

-- |
-- Convert a path to a list of database operations for deletion.
--
-- Traverses the path bottom-up, computing updated hashes and generating
-- insert\/delete operations.
--
-- == Operations generated
--
-- * @(key, Nothing)@ - Delete the node at @key@
-- * @(key, Just indirect)@ - Insert or update the node at @key@
--
-- == Tree compaction
--
-- When a branch loses one child (the deleted node), the remaining sibling's
-- jump path is extended to absorb the now-unnecessary branch point. This
-- maintains the compact representation where only necessary branch points exist.
--
-- == Example
--
-- For a path to key @[L, R]@ in a tree with sibling at @[L, L]@:
--
-- @
-- pathToOps hashing path
-- -- Returns operations to:
-- --   1. Delete [L, R]
-- --   2. Update root to point directly to [L, L] with extended jump
-- --   3. Delete the old [L] branch node
-- @
pathToOps
    :: forall a
     . Hashing a
    -- ^ Hash functions for computing updated node values
    -> TreePath a
    -- ^ The path to convert
    -> [(Key, Maybe (Indirect a))]
    -- ^ List of (key, operation) pairs to apply atomically
pathToOps hashing = snd . go []
  where
    go
        :: Key
        -> TreePath a
        -> (Maybe (Indirect a), [(Key, Maybe (Indirect a))])
    go k (PathLeaf _ _v) = (Nothing, [(k, Nothing)])
    go k (PathBranch j d v i) =
        let
            (msb, xs) = go (k <> j <> [d]) v
        in
            case msb of
                Just i' ->
                    let h = addWithDirection hashing d i' i
                        i'' = Indirect{jump = j, value = h}
                    in  ( Just i''
                        , [(k, Just i'')] <> xs
                        )
                Nothing ->
                    let i' =
                            Indirect
                                { jump = j <> [oppositeDirection d] <> jump i
                                , value = value i
                                }
                    in  ( Just i'
                        , [ (k, Just i')
                          , (k <> j <> [oppositeDirection d], Nothing)
                          ]
                            <> xs
                        )

-- |
-- Convert a path to proof steps for Merkle inclusion proofs.
--
-- Returns the components needed to construct an 'InclusionProof':
--
-- == Return value
--
-- A tuple of @(rootJump, leafValue, steps)@ where:
--
-- * @rootJump@ - The jump path at the root node
-- * @leafValue@ - The value stored at the target leaf
-- * @steps@ - Proof steps ordered from leaf to root
--
-- == Step format
--
-- Each step is @(bitsConsumed, sibling)@ where:
--
-- * @bitsConsumed@ - Number of key bits used at this level (1 + jump length)
-- * @sibling@ - The sibling's indirect reference for hash verification
--
-- == Ordering
--
-- Steps are ordered leaf-to-root to match the verification algorithm in
-- 'CSMT.Proof.Insertion.computeRootHash', which processes the key from
-- the leaf end backward toward the root.
pathToProofSteps
    :: TreePath a
    -> (Key, a, [(Int, Indirect a)])
    -- ^ @(rootJump, leafValue, steps)@ - components for proof construction
pathToProofSteps path = (rootJump, leafVal, reverse revSteps)
  where
    (rootJump, leafVal, revSteps) = go path

    go (PathLeaf nodeJump leafValue) = (nodeJump, leafValue, [])
    go (PathBranch nodeJump _dir child sibling) =
        let (childJump, childLeafVal, childRevSteps) = go child
            -- Bits consumed = 1 (for direction) + length of child's jump
            consumed = 1 + length childJump
            step = (consumed, sibling)
        in  (nodeJump, childLeafVal, step : childRevSteps)
