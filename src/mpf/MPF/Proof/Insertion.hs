{-# LANGUAGE StrictData #-}

module MPF.Proof.Insertion
    ( MPFProof (..)
    , MPFProofStep (..)
    , MerkleProofItem (..)
    , mkMPFInclusionProof
    , foldMPFProof
    , verifyMPFInclusionProof
    )
where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Foldable (foldl')
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )
import MPF.Hashes (MPFHashing (..))
import MPF.Interface
    ( FromHexKV (..)
    , HexDigit (..)
    , HexIndirect (..)
    , HexKey
    )

-- | A single item in a merkle proof
-- Indicates whether this is a left or right sibling in the merkle tree
data MerkleProofItem a
    = -- | Hash of left sibling
      MPILeft a
    | -- | Hash of right sibling
      MPIRight a
    | -- | No sibling (for sparse positions)
      MPINone
    deriving (Show, Eq)

-- | Proof step types for MPF
data MPFProofStep a
    = ProofStepLeaf
        { pslPrefixLen :: Int
        -- ^ Length of common prefix
        , pslNeighborKeyPath :: HexKey
        -- ^ Neighbor's remaining key path
        , pslNeighborValueDigest :: a
        -- ^ Neighbor's value digest
        }
    | ProofStepFork
        { psfPrefixLen :: Int
        -- ^ Length of common prefix
        , psfNeighborPrefix :: HexKey
        -- ^ Neighbor branch prefix
        , psfNeighborIndex :: HexDigit
        -- ^ Neighbor's position
        , psfMerkleRoot :: a
        -- ^ Merkle root of branch
        }
    | ProofStepBranch
        { psbPrefixLen :: Int
        -- ^ Length of common prefix
        , psbSiblingHashes :: [(HexDigit, a)]
        -- ^ Sibling hashes at this branch
        }
    deriving (Show, Eq)

-- | Complete membership proof for MPF
data MPFProof a = MPFProof
    { mpfProofSteps :: [MPFProofStep a]
    , mpfProofRootPrefix :: HexKey
    }
    deriving (Show, Eq)

-- | Generate a membership proof for a key
mkMPFInclusionProof
    :: (Monad m, GCompare d)
    => FromHexKV k v a
    -> Selector d HexKey (HexIndirect a)
    -> k
    -> Transaction m cf d ops (Maybe (MPFProof a))
mkMPFInclusionProof FromHexKV{fromHexK} sel k = runMaybeT $ do
    let key = fromHexK k
    HexIndirect{hexJump = rootJump, hexIsLeaf = _} <- MaybeT $ query sel []
    guard $ isPrefixOf rootJump key
    steps <- go rootJump $ drop (length rootJump) key
    pure
        $ MPFProof{mpfProofSteps = reverse steps, mpfProofRootPrefix = rootJump}
  where
    go _ [] = pure []
    go u (x : ks) = do
        HexIndirect{hexJump = jump} <- MaybeT $ query sel (u <> [x])
        guard $ isPrefixOf jump ks
        -- Collect sibling information at this branch
        siblings <- MaybeT $ Just <$> fetchSiblings sel u x
        let step =
                ProofStepBranch
                    { psbPrefixLen = length jump
                    , psbSiblingHashes = Map.toList $ Map.map hexValue siblings
                    }
        (step :) <$> go (u <> (x : jump)) (drop (length jump) ks)

-- | Fetch all sibling nodes at a branch point (excluding the given digit)
fetchSiblings
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexDigit
    -> Transaction m cf d ops (Map HexDigit (HexIndirect a))
fetchSiblings sel prefix exclude = do
    let digits = [HexDigit n | n <- [0 .. 15], HexDigit n /= exclude]
    pairs <- mapM fetchOne digits
    pure $ Map.fromList [(d, i) | (d, Just i) <- pairs]
  where
    fetchOne d = do
        mi <- query sel (prefix <> [d])
        pure (d, mi)

-- | Fold a proof to compute the root hash
foldMPFProof :: MPFHashing a -> a -> MPFProof a -> a
foldMPFProof hashing valueHash MPFProof{mpfProofSteps, mpfProofRootPrefix} =
    branchHash hashing mpfProofRootPrefix rootValue
  where
    rootValue = foldl' step (leafHash hashing [] valueHash) mpfProofSteps
    step acc proofStep =
        case proofStep of
            ProofStepBranch{psbPrefixLen, psbSiblingHashes} ->
                let jump = replicate psbPrefixLen (HexDigit 0) -- Placeholder
                    siblingMap = Map.fromList psbSiblingHashes
                    -- Build sparse array including our value
                    sparseArray = [Map.lookup (HexDigit n) siblingMap | n <- [0 .. 15]]
                    -- Note: This is simplified; actual implementation needs to track position
                    mr = merkleRoot hashing sparseArray
                in  branchHash hashing jump mr
            ProofStepLeaf{} ->
                -- Leaf step: handled at termination
                acc
            ProofStepFork{psfMerkleRoot} ->
                -- Fork step: use the provided merkle root
                psfMerkleRoot

-- | Verify a membership proof
verifyMPFInclusionProof
    :: (Eq a, Monad m, GCompare d)
    => FromHexKV k v a
    -> Selector d HexKey (HexIndirect a)
    -> MPFHashing a
    -> v
    -> MPFProof a
    -> Transaction m cf d ops Bool
verifyMPFInclusionProof FromHexKV{fromHexV} sel hashing v proof = do
    let valueHash = fromHexV v
    mv <- query sel []
    pure $ case mv of
        Just rootValue ->
            hexValue rootValue == foldMPFProof hashing valueHash proof
        Nothing -> False
