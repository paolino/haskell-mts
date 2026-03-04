-- | Generic rollback library for key-value stores.
--
-- Every mutation (insert, delete, update) is modeled
-- as a swap: bring a new binding, exchange it with
-- whatever the state currently holds at that key. The
-- displaced binding is the inverse operation.
--
-- Rollback replays the inverse log in reverse order,
-- restoring the original state. This is proved correct
-- in @lean\/Rollbacks\/Rollback.lean@.
--
-- For types, see "MTS.Rollbacks.Types".
-- For column GADT, see "MTS.Rollbacks.Column".
-- For operations, see "MTS.Rollbacks.Store".
module MTS.Rollbacks
    ( -- * Types
      module MTS.Rollbacks.Types

      -- * Column
    , module MTS.Rollbacks.Column

      -- * Store operations
    , module MTS.Rollbacks.Store
    )
where

import MTS.Rollbacks.Column
import MTS.Rollbacks.Store
import MTS.Rollbacks.Types
