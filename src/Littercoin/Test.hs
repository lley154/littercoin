{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Littercoin.Test
    (
        runTest
    )

where
    
--import           Control.Lens                         (review)
--import           Control.Monad                        (forever)
--import           Data.Aeson                           (FromJSON, ToJSON)
--import           Data.Text                            (Text)
--import qualified Data.Map                             as Map
--import           Data.Monoid                          (Last (..))
--import qualified Data.OpenApi                         as OpenApi
import           Data.Void                            (Void)
import           GHC.Generics                         (Generic)
import qualified Plutus.Trace.Emulator                as Emulator
--import           Ledger                               (getCardanoTxId)
--import qualified Ledger.Ada                           as Ada
import qualified Ledger.Address                       as Address
import qualified Ledger.CardanoWallet                 as CardanoWallet
--import qualified Ledger.Constraints                   as Constraints
--import           Ledger.Params                        (Params)
--import qualified Ledger.Tx                            as Tx
--import qualified Ledger.Value                         as Value
import           Littercoin.OffChain
import           Littercoin.Types
import           Playground.Schema                    (endpointsToSchemas)
import qualified Plutus.Contract                      as Contract
import qualified Plutus.Contract.Request              as Request
import qualified Plutus.Contract.Wallet               as Wallet
--import qualified Plutus.PAB.Effects.Contract.Builtin  as Builtin
--import qualified Plutus.Script.Utils.Typed            as Typed
--import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
--import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
--import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as ValidatorsV2
--import qualified Plutus.V2.Ledger.Api                 as PlutusV2
--import qualified Plutus.V2.Ledger.Contexts            as ContextsV2
--import qualified Plutus.V2.Ledger.Tx                  as TxV2
--import qualified PlutusTx
import           PlutusPrelude                        (Pretty, void)                             
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       Show (..), String, print, (.),
                                                       Either(..), return)
import qualified Wallet.Emulator.Wallet                as Wallet


adminPkh1 :: Address.PaymentPubKeyHash
adminPkh1 = CardanoWallet.paymentPubKeyHash (CardanoWallet.fromWalletNumber $ CardanoWallet.WalletNumber 1)

runTest :: IO ()
runTest = Emulator.runEmulatorTraceIO $ myTrace 

myTrace :: Emulator.EmulatorTrace ()
myTrace = do

    --Contract.logInfo @String "starting lotto"


    let w1 = Wallet.knownWallet 1
        w2 = Wallet.knownWallet 2
        tokenName = "Littercoin"
        nftTokenName = "Littercoin Approved Merchant"
        qty1 = 100
        qty2 = 25
        adaAmount = 10000000 -- 10 Ada

    -- lotto admin wallet    
    initHandle <- Emulator.activateContractWallet (Wallet.knownWallet 1) initEndpoint
 
    void $ Emulator.callEndpoint @"init" initHandle TokenParams
        { tpLCTokenName = tokenName
        , tpNFTTokenName = nftTokenName
        , tpQty = qty1  -- ignored for init
        , tpAdminPkh = adminPkh1
        }
    void $ Emulator.waitNSlots 2




