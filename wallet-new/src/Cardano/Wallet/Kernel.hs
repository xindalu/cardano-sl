{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Cardano.Wallet.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , WalletId
  , walletLogMessage
  , walletPassive
  , getWalletUtxo
  , getWalletPending
  , bracketPassiveWallet
  , init
  , newWalletHdRnd
  , applyBlock
  , applyBlocks
  , availableBalance
  , totalBalance
  , stateUtxo
  , stateUtxoBalance
  , wallets'
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  , hasPending
  ) where

import           Universum hiding (State)

import           Control.Lens.TH
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust) -- TODO remove with refactor to AdicState Wallet
import qualified Data.Set as Set

import           System.Wlog (Severity (..))

import           Data.Acid (AcidState)
import           Data.Acid.Memory (openMemoryState)

import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.PrefilterTx (ourUtxo, prefilterBlock)
import           Cardano.Wallet.Kernel.Types (txUtxo)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, defDB)
import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Util as Spec

import           Pos.Core (HasConfiguration, TxAux (..))
import           Pos.Crypto (EncryptedSecretKey, hash)
import           Pos.Txp (Utxo)
import           Pos.Util.Chrono (NE, OldestFirst)
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..))

{-------------------------------------------------------------------------------
  Wallet with State
-------------------------------------------------------------------------------}

-- | Wallet State
data State = State {
      _stateUtxo        :: Utxo
    , _statePending     :: Spec.PendingTxs
    , _stateUtxoBalance :: Spec.Balance
    }

-- | Wallet
--
-- Contains the Wallet EncryptedSecretKey and State DB handle
data Wallet = WalletHdRnd {
      -- | Wallet master key
      --
      -- TODO: We may need to rethink having this in-memory
      -- ESK should _not_ end up in the wallet's acid-state log
      -- (for some reason..)
    _walletESK     :: EncryptedSecretKey

    -- | Wallet state
    --
    -- TODO: will become an acid-state handle
    , _walletState :: MVar State
    }

-- | Wallet Id
--
-- A Wallet Id can take several forms, the simplest of which is a hash
-- of the Wallet public key
data WalletId =
    -- | HD wallet with randomly generated addresses
    WalletIdHdRnd Int -- TODO use (Hash PublicKey), this will be fixed with the Acidstate design

    {- potential future kinds of wallet IDs:
    -- | HD wallet with sequentially generated addresses
    | WalletIdHdSeq ...

    -- | External wallet (all crypto done off-site, like hardware wallets)
    | WalletIdExt ...
    -}

    deriving (Eq, Ord)

-- | Wallets
--
-- Map of Wallets indexed by WalletId
type Wallets = Map WalletId Wallet

makeLenses ''Wallet
makeLenses ''State

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
--
data PassiveWallet = PassiveWallet {
      -- | Send log message
      _walletLogMessage :: Severity -> Text -> IO ()
    , _wallets          :: MVar Wallets -- TODO remove
    , _wallets'          :: AcidState DB
    }

makeLenses ''PassiveWallet

{-------------------------------------------------------------------------------
  Passive Wallet Resource Management
-------------------------------------------------------------------------------}

-- | Allocate wallet resources
--
-- Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: (MonadMask m, MonadIO m)
                     => (Severity -> Text -> IO ())
                     -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet _walletLogMessage f =
    bracket (liftIO $ openMemoryState defDB)
            (\_ -> return ())
            (\db ->
                bracket
                  (liftIO $ initPassiveWallet _walletLogMessage db)
                  (\_ -> return ())
                  f)

{-------------------------------------------------------------------------------
TODO move to Spec.READ/UTIL
-------------------------------------------------------------------------------}

available :: PassiveWallet -> WalletId -> IO Utxo
available pw wid = do
    State utxo pending _ <- getWalletState pw wid

    return $ Spec.utxoRemoveInputs utxo (Spec.txIns pending)

change :: PassiveWallet -> WalletId -> IO Utxo
change pw wid = do
    State _ pending _ <- getWalletState pw wid
    let pendingUtxo = Spec.unionTxOuts $ map (txUtxo . taTx) $ Map.elems pending

    w <- fromJust <$> findWallet pw wid
    return $ ourUtxo (w ^. walletESK) pendingUtxo

_total :: PassiveWallet -> WalletId -> IO Utxo
_total pw wid = Map.union <$> available pw wid <*> change pw wid

availableBalance :: PassiveWallet -> WalletId -> IO Spec.Balance
availableBalance pw wid = do
    State utxo pending utxoBalance <- getWalletState pw wid
    let balanceDelta = Spec.balance (Spec.utxoRestrictToInputs utxo (Spec.txIns pending))
    return $ utxoBalance - balanceDelta

totalBalance :: PassiveWallet -> WalletId -> IO Spec.Balance
totalBalance pw wid = do
    availableBalance' <- availableBalance pw wid
    changeBalance' <- Spec.balance <$> change pw wid
    return $ availableBalance' + changeBalance'

{-------------------------------------------------------------------------------
  Passive wallet - tmp DB -- TODO remove
-------------------------------------------------------------------------------}

getWallets :: PassiveWallet -> IO Wallets
getWallets pw = modifyWallets pw identity

-- | Modify PassiveWallet Wallet collection
modifyWallets :: PassiveWallet -> (Wallets -> Wallets) -> IO Wallets
modifyWallets pw modifyF = do
    ws <- takeMVar mvar

    let ws' = modifyF ws
    putMVar mvar ws'
    return ws'
    where mvar = pw ^. wallets

-- | Insert a new {WalletId -> Wallet} into PassiveWallet wallets
insertWallet :: PassiveWallet -> WalletId -> Wallet -> IO ()
insertWallet pw wid w
    = void $ modifyWallets pw (Map.insert wid w)

-- | Lookup Wallet in PassiveWallet using WalletId
findWallet :: PassiveWallet -> WalletId -> IO (Maybe Wallet)
findWallet pw wid = do
    wallets_ <- getWallets pw
    return $ Map.lookup wid wallets_

-- | Return the wallet's current State
getWalletState :: PassiveWallet -> WalletId -> IO State
getWalletState pw wid = modifyWalletState pw wid identity

-- | Return the wallet's current UTxO
getWalletUtxo :: PassiveWallet -> WalletId -> IO Utxo
getWalletUtxo pw wid = _stateUtxo <$> getWalletState pw wid

-- | Return the wallet's current Pending
getWalletPending :: PassiveWallet -> WalletId -> IO Spec.PendingTxs
getWalletPending pw wid = _statePending <$> getWalletState pw wid

-- | Modify Wallet State with given modifier function
modifyWalletState :: PassiveWallet -> WalletId -> (State -> State) -> IO State
modifyWalletState pw wid modifyF = do
    w <- fromJust <$> findWallet pw wid
    let mvar = w ^. walletState
    s <- takeMVar mvar

    let s' = modifyF s
    putMVar mvar s'
    return s'

-- | Replace Wallet State
updateWalletState :: PassiveWallet -> WalletId -> State -> IO ()
updateWalletState pw wid s' = void $ modifyWalletState pw wid (const s')

-- | Insert a new pending transaction to Pending set
--   for the Wallet given by WalletId
insertWalletPending :: ActiveWallet -> WalletId -> TxAux -> IO ()
insertWalletPending ActiveWallet{..} wid tx
    = void $ modifyWalletState walletPassive wid modifyF
    where modifyF = over statePending (Map.insert txId tx)
          txId = hash $ taTx tx

{-------------------------------------------------------------------------------
  Wallet Initialisers
-------------------------------------------------------------------------------}

-- | Initialise Passive Wallet with empty Wallets collection
initPassiveWallet :: (Severity -> Text -> IO ())
                  -> AcidState DB
                  -> IO PassiveWallet
initPassiveWallet logMessage db = do
    ws <- Universum.newMVar Map.empty
    return $ PassiveWallet logMessage ws db

-- | Initialise Wallet with ESK and empty State
initWalletHdRnd :: EncryptedSecretKey -> Utxo -> IO Wallet
initWalletHdRnd esk utxo = do
    state' <- Universum.newMVar initState
    return $ WalletHdRnd esk state'
    where
        initState :: State
        initState = State { _stateUtxo = utxo
                          , _statePending = Map.empty
                          -- TODO Prefilter transactions (currently used only in tests,
                          --      which does prefiltering)
                          , _stateUtxoBalance = Spec.balance utxo
                          }

-- | Add a new WalletHdRnd, with given ESK, to the PassiveWallet collection of
--   Wallets. The new WalletID is generated from the ESK.
newWalletHdRnd :: PassiveWallet -> EncryptedSecretKey -> Utxo -> IO WalletId
newWalletHdRnd pw@PassiveWallet{..} esk utxo = do
    ws <- getWallets pw
    let wid = WalletIdHdRnd (Map.size ws) -- TODO will be fixed once we move to the AcidState design

    w <- initWalletHdRnd esk utxo
    insertWallet pw wid w
    return wid

-- | Initialize the Passive wallet (specified by the ESK) with the given Utxo
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} = _walletLogMessage Info "Passive Wallet kernel initialized"

{-------------------------------------------------------------------------------
  Passive Wallet API implementation: applyBlock, available, change, balances
-------------------------------------------------------------------------------}

-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: HasConfiguration
          => PassiveWallet
          -> ResolvedBlock
          -> IO ()
applyBlock pw b
    = do
        ws <- getWallets pw
        let wids = map fst $ Map.toList ws
        mapM_ (applyBlock' pw b) wids

-- | Apply the ResolvedBlock to the PassiveWallet indexed by WalletID @wid@
applyBlock' :: HasConfiguration
            => PassiveWallet
            -> ResolvedBlock
            -> WalletId
            -> IO ()
applyBlock' pw b wid = do
    (State utxo' pending' balance') <- getWalletState pw wid
    w <- fromJust <$> findWallet pw wid

    let prefBlock              = prefilterBlock (w ^. walletESK) b
        (utxo'', balanceDelta) = updateUtxo_    prefBlock utxo'
        pending''              = Spec.updatePending prefBlock pending'
        balance''              = balance' + balanceDelta

    updateWalletState pw wid $ State utxo'' pending'' balance''

-- | Apply the ResolvedBlocks, one at a time, to all wallets in the PassiveWallet
applyBlocks :: HasConfiguration
              => PassiveWallet
              -> OldestFirst NE ResolvedBlock
              -> IO ()
applyBlocks pw = mapM_ (applyBlock pw)

updateUtxo_ :: PrefilteredBlock -> Utxo -> (Utxo, Spec.Balance)
updateUtxo_ PrefilteredBlock{..} currentUtxo =
      (utxo', balanceDelta)
    where
        unionUtxo = Map.union pfbOutputs currentUtxo
        utxo' = Spec.utxoRemoveInputs unionUtxo pfbInputs
        unionUtxoRestricted  = Spec.utxoRestrictToInputs unionUtxo pfbInputs
        balanceDelta = Spec.balance pfbOutputs - Spec.balance unionUtxoRestricted

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive   :: PassiveWallet

      -- | The wallet diffusion layer
    , walletDiffusion :: WalletDiffusion
    }

-- | Initialize the active wallet
bracketActiveWallet :: MonadMask m
                    => PassiveWallet
                    -> WalletDiffusion
                    -> (ActiveWallet -> m a) -> m a
bracketActiveWallet walletPassive walletDiffusion =
    bracket
      (return ActiveWallet{..})
      (\_ -> return ())

{-------------------------------------------------------------------------------
  Active wallet API implementation
-------------------------------------------------------------------------------}

-- | Return True if there are pending transactions
hasPending :: ActiveWallet -> WalletId -> IO Bool
hasPending ActiveWallet{..} wid = do
    s <- getWalletState walletPassive wid
    return $ Map.size (s ^. statePending) > 0

-- | Submit a new pending transaction
newPending :: ActiveWallet -> WalletId -> TxAux -> IO Bool
newPending activeWallet@ActiveWallet{..} wid tx = do
    availableInputs <- Spec.utxoInputs <$> available walletPassive wid

    let isValid = Spec.txAuxInputSet tx `Set.isSubsetOf` availableInputs
    when isValid $ insertWalletPending activeWallet wid tx
    return isValid
