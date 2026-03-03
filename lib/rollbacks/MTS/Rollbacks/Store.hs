-- | Transaction-level rollback operations.
--
-- All functions operate on 'Transaction' with
-- 'RollbackColumn' — downstream consumers use
-- 'liftRollback' to embed them into larger
-- column GADTs.
--
-- The library stores and retrieves inverse
-- operations but does not know how to /apply/
-- them. Rollback functions take a callback
-- for inverse application.
module MTS.Rollbacks.Store
    ( -- * Forward (store rollback point)
      storeRollbackPoint

      -- * Tip query
    , queryTip

      -- * Rollback
    , RollbackResult (..)
    , rollbackTo

      -- * Finality (prune old points)
    , pruneBelow

      -- * Armageddon (full cleanup)
    , armageddonCleanup
    , armageddonSetup

      -- * Inspection
    , countPoints
    )
where

import Control.Monad.Trans.Class (lift)
import Data.Function (fix)
import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , lastEntry
    , nextEntry
    , prevEntry
    , seekKey
    )
import Database.KV.Transaction
    ( Transaction
    , delete
    , insert
    , iterating
    )
import MTS.Rollbacks.Column
    ( RollbackColumn (..)
    )
import MTS.Rollbacks.Types
    ( RollbackPoint (..)
    , WithOrigin (..)
    )

-- | Store a rollback point at the given slot.
--
-- Call this during forward-tip processing after
-- computing inverse operations.
storeRollbackPoint
    :: (Ord slot)
    => slot
    -- ^ Slot of the block being applied
    -> RollbackPoint inv meta
    -- ^ Inverses and metadata for this slot
    -> Transaction
        m
        cf
        (RollbackColumn slot inv meta)
        op
        ()
storeRollbackPoint slot rp =
    insert RollbackPoints (At slot) rp

-- | Query the current tip slot.
--
-- Returns 'Origin' if only the genesis point
-- exists. Fails if no rollback points exist
-- (database not initialized).
queryTip
    :: (Monad m)
    => Transaction
        m
        cf
        (RollbackColumn slot inv meta)
        op
        (Maybe (WithOrigin slot))
queryTip =
    iterating RollbackPoints $ do
        ml <- lastEntry
        pure $ fmap entryKey ml

-- | Result of a rollback attempt.
data RollbackResult
    = -- | Rollback succeeded. The 'Int' is the
      -- number of points deleted.
      RollbackSucceeded Int
    | -- | Target slot not found. Database must
      -- be truncated (armageddon).
      RollbackImpossible
    deriving stock (Eq, Show)

-- | Roll back to the given slot.
--
-- Iterates backward from the tip, calling the
-- provided callback for each rollback point
-- strictly after the target slot. Points after
-- the target are deleted; the target's point
-- is kept.
--
-- The callback receives each 'RollbackPoint' in
-- reverse chronological order (most recent first).
rollbackTo
    :: (Ord slot, Monad m)
    => ( RollbackPoint inv meta
         -> Transaction
                m
                cf
                (RollbackColumn slot inv meta)
                op
                ()
       )
    -- ^ Apply inverses from one rollback point
    -> slot
    -- ^ Target slot to roll back to
    -> Transaction
        m
        cf
        (RollbackColumn slot inv meta)
        op
        RollbackResult
rollbackTo applyInverses targetSlot =
    iterating RollbackPoints $ do
        mTarget <- seekKey (At targetSlot)
        case mTarget of
            Nothing -> pure RollbackImpossible
            Just Entry{entryKey}
                | entryKey /= At targetSlot ->
                    pure RollbackImpossible
                | otherwise -> do
                    -- Target found, now walk from tip
                    -- backward deleting everything
                    -- strictly after target
                    ml <- lastEntry
                    n <- walkBack ml
                    pure (RollbackSucceeded n)
  where
    walkBack cur =
        ($ cur) $ fix $ \go -> \case
            Nothing -> pure 0
            Just Entry{entryKey, entryValue}
                | entryKey > At targetSlot -> do
                    lift
                        $ applyInverses entryValue
                    lift
                        $ delete
                            RollbackPoints
                            entryKey
                    prev <- prevEntry
                    (+ 1) <$> go prev
                | otherwise -> pure 0

-- | Prune all rollback points strictly before
-- the given finality slot. Returns the number
-- of points pruned.
pruneBelow
    :: (Ord slot, Monad m)
    => slot
    -- ^ Finality slot (exclusive lower bound)
    -> Transaction
        m
        cf
        (RollbackColumn slot inv meta)
        op
        Int
pruneBelow slot =
    iterating RollbackPoints $ do
        me <- firstEntry
        ($ me) $ fix $ \go -> \case
            Nothing -> pure 0
            Just Entry{entryKey}
                | entryKey < At slot -> do
                    lift
                        $ delete
                            RollbackPoints
                            entryKey
                    next <- nextEntry
                    (+ 1) <$> go next
                | otherwise -> pure 0

-- | Delete rollback points in a batch. Returns
-- 'True' if more entries remain (caller should
-- loop).
--
-- This is for armageddon (full DB reset) when
-- rollback is impossible. Run in a loop with
-- a transaction runner.
armageddonCleanup
    :: (Ord slot, Monad m)
    => Int
    -- ^ Batch size (entries per transaction)
    -> Transaction
        m
        cf
        (RollbackColumn slot inv meta)
        op
        Bool
armageddonCleanup batchSz =
    iterating RollbackPoints $ do
        me <- firstEntry
        ($ (me, 0 :: Int)) $ fix $ \go -> \case
            (Nothing, _) -> pure False
            (_, n) | n >= batchSz -> pure True
            (Just Entry{entryKey}, n) -> do
                lift
                    $ delete
                        RollbackPoints
                        entryKey
                next <- nextEntry
                go (next, n + 1)

-- | Initialize the rollback column with a single
-- 'Origin' point carrying empty inverses.
--
-- Call after 'armageddonCleanup' completes, or
-- on fresh database setup.
armageddonSetup
    :: (Ord slot)
    => Maybe meta
    -- ^ Optional metadata for the origin point
    -> Transaction
        m
        cf
        (RollbackColumn slot inv meta)
        op
        ()
armageddonSetup meta =
    insert RollbackPoints Origin
        $ RollbackPoint
            { rpInverses = []
            , rpMeta = meta
            }

-- | Count total rollback points.
countPoints
    :: (Monad m)
    => Transaction
        m
        cf
        (RollbackColumn slot inv meta)
        op
        Int
countPoints =
    iterating RollbackPoints $ do
        me <- firstEntry
        ($ me) $ fix $ \go -> \case
            Nothing -> pure 0
            Just _ -> do
                next <- nextEntry
                (+ 1) <$> go next
