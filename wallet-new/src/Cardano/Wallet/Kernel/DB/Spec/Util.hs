-- | UPDATE operations on the wallet-spec state
module Cardano.Wallet.Kernel.DB.Spec.Util (
    PendingTxs
  , Balance
  , disjoint
  , balance
  , txIns
  , utxoInputs
  , unionTxOuts
  , utxoRemoveInputs
  , utxoRestrictToInputs
  , txAuxInputSet
  ) where

import           Universum

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

import qualified Pos.Core as Core

import           Pos.Txp (Utxo)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxIn (..), TxOut (..), TxOutAux (..))

import           Cardano.Wallet.Kernel.DB.Spec

utxoOutputs :: Utxo -> [TxOut]
utxoOutputs = map toaOut . Map.elems

balance :: Utxo -> Balance
balance = Core.sumCoins . map txOutValue . utxoOutputs

unionTxOuts :: [Utxo] -> Utxo
unionTxOuts = Map.unions

utxoInputs :: Utxo -> Set TxIn
utxoInputs = Map.keysSet

txIns :: PendingTxs -> Set TxIn
txIns = Set.fromList . concatMap (NE.toList . _txInputs . taTx) . Map.elems

txAuxInputSet :: TxAux -> Set TxIn
txAuxInputSet = Set.fromList . NE.toList . _txInputs . taTx

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys` s = m `Map.difference` Map.fromSet (const ()) s

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
m `restrictKeys` s = m `Map.intersection` Map.fromSet (const ()) s

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null (a `Set.intersection` b)

utxoRemoveInputs :: Utxo -> Set TxIn -> Utxo
utxoRemoveInputs = withoutKeys

utxoRestrictToInputs :: Utxo -> Set TxIn -> Utxo
utxoRestrictToInputs = restrictKeys
