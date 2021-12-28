{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Main where

import Playground.Contract
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Data.Functor (void)
import Ledger
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Crypto
import Ledger.CardanoWallet (MockWallet)
import Ledger.CardanoWallet qualified as CardanoWallet
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell
import Schema (ToSchema)
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Emulator.Wallet qualified as Wallet
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Types as WalletTypes

data TransferData = 
    TransferData 
        { to     :: PubKeyHash
        , amount :: Ada
        }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''TransferData
PlutusTx.makeLift ''TransferData

{-# INLINABLE validateTransfer #-}
validateTransfer :: PubKeyHash -> () -> ScriptContext -> Bool
validateTransfer _ _ _ = Haskell.True

data Transfer
instance Scripts.ValidatorTypes Transfer where
    type instance RedeemerType Transfer = ()
    type instance DatumType Transfer    = PubKeyHash

transferValidator :: Scripts.TypedValidator Transfer
transferValidator = Scripts.mkTypedValidator @Transfer
    $$(PlutusTx.compile [|| validateTransfer ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @PubKeyHash @()

type TransferSchema =
        Endpoint "transfer" TransferData

transfer :: Promise () TransferSchema T.Text ()
transfer = endpoint @"transfer" transferFunds

transferFunds :: TransferData -> Contract () TransferSchema T.Text ()
transferFunds TransferData{..} = do
    logInfo $ "Transfering " <> Haskell.show amount <> " To " <> Haskell.show to
    let tx = Constraints.mustPayToPubKey to (Ada.toValue amount)
    void $ submitTxConstraints transferValidator tx

endpoints :: Contract () TransferSchema T.Text ()
endpoints = selectList [transfer]

mkSchemaDefinitions ''TransferSchema

$(mkKnownCurrencies [])

toMockWallet :: Wallet -> MockWallet
toMockWallet w = fromMaybe err $ Wallet.walletMockWallet w
  where
    err :: MockWallet
    err = error ()
    -- err = error $ "Wallet " <> Haskell.show w <> "is not a mock wallet."

walletPubKey :: Wallet -> PubKey
walletPubKey = CardanoWallet.pubKey . toMockWallet

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (WalletTypes.fromWalletNumber . WalletNumber $ 1) endpoints
    callEndpoint @"transfer" h1 $ TransferData
        { to     = pubKeyHash . walletPubKey $ (WalletTypes.fromWalletNumber . WalletNumber $ 2)
        , amount = Ada.lovelaceOf 1000
        }
    void $ waitUntilSlot 20

main :: Haskell.IO ()
main = runEmulatorTraceIO myTrace
