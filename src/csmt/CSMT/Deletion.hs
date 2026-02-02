-- |
-- Module      : CSMT.Deletion
-- Description : Deletion algorithm for Compact Sparse Merkle Trees
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module implements the deletion algorithm for CSMTs.
--
-- The deletion process:
--
-- 1. Build a 'DeletionPath' representing the path to the value being deleted
-- 2. Convert the path to database operations (deletions and updates)
-- 3. Apply all operations atomically
--
-- When a value is deleted, the tree structure may need to be compacted
-- by extending jump paths where branches become unnecessary.
module CSMT.Deletion
    ( deleting
    , newDeletionPath
    , DeletionPath
    , deletionPathToOps
    )
where

import CSMT.Interface
    ( FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    )
import CSMT.Path
    ( TreePath (..)
    , extractPath
    , pathToOps
    )
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , delete
    , insert
    )

-- |
-- A path through the tree to a value being deleted.
--
-- This is now a type alias for 'TreePath' from "CSMT.Path".
-- The constructors are:
--
-- * 'PathLeaf' - The leaf node containing the value to delete
-- * 'PathBranch' - An internal node along the path, recording the sibling
--   that will need to be updated after deletion
type DeletionPath a = TreePath a

-- |
-- Delete a key-value pair from the CSMT.
--
-- This function:
--
-- 1. Builds a deletion path from root to the target key
-- 2. Removes the key from the KV store
-- 3. Updates the tree structure and recomputes affected hashes
deleting
    :: (Monad m, Ord k, GCompare d)
    => FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> k
    -> Transaction m cf d ops ()
deleting FromKV{fromK} hashing kvSel csmtSel key = do
    mpath <- newDeletionPath csmtSel (fromK key)
    case mpath of
        Nothing -> pure ()
        Just path -> do
            delete kvSel key
            mapM_ (applyOp csmtSel) $ deletionPathToOps hashing path

-- | Apply a single database operation (insert or delete).
applyOp
    :: GCompare d
    => Selector d Key (Indirect a)
    -> (Key, Maybe (Indirect a))
    -> Transaction m cf d ops ()
applyOp csmtSel (k, Nothing) = delete csmtSel k
applyOp csmtSel (k, Just i) = insert csmtSel k i

-- |
-- Convert a deletion path to a list of database operations.
--
-- Traverses the path bottom-up, computing updated hashes and generating
-- insert/delete operations. When a branch loses one child, its sibling's
-- jump path is extended to maintain the compact representation.
deletionPathToOps
    :: Hashing a
    -> DeletionPath a
    -> [(Key, Maybe (Indirect a))]
deletionPathToOps = pathToOps

-- |
-- Build a deletion path for the given key.
--
-- Traverses the tree from root to the target key, recording each branch
-- along the way. Returns 'Nothing' if the key does not exist in the tree.
newDeletionPath
    :: (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -> Transaction m cf d ops (Maybe (DeletionPath a))
newDeletionPath = extractPath
