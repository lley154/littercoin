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
    

import qualified Plutus.Trace.Emulator                as Emulator
import qualified Ledger.Address                       as Address
import qualified Ledger.CardanoWallet                 as CardanoWallet
import           Littercoin.OffChain
import           Littercoin.Types                     (TokenParams(..))
import           Control.Monad.Freer.Extras.Log       (logInfo)
import           PlutusPrelude                        (void)                             
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Show (..), String)
import qualified Wallet.Emulator.Wallet                as Wallet


adminPkh1 :: Address.PaymentPubKeyHash
adminPkh1 = CardanoWallet.paymentPubKeyHash (CardanoWallet.fromWalletNumber $ CardanoWallet.WalletNumber 1)

runTest :: IO ()
runTest = Emulator.runEmulatorTraceIO $ myTrace 

myTrace :: Emulator.EmulatorTrace ()
myTrace = do

    logInfo @String "starting lotto"


    let w1 = Wallet.knownWallet 1
        w2 = Wallet.knownWallet 2
        tokenName = "Littercoin"
        nftTokenName = "Littercoin Approved Merchant"
        qty1 = 100 :: Integer
        qty2 = 25 :: Integer
        adaAmount = 10000000 :: Integer -- 10 Ada

    -- lotto admin wallet    
    initHandle <- Emulator.activateContractWallet (Wallet.knownWallet 1) initEndpoint
 
    void $ Emulator.callEndpoint @"init" initHandle TokenParams
        { tpLCTokenName = tokenName
        , tpNFTTokenName = nftTokenName
        , tpQty = qty1  -- ignored for init
        , tpAdminPkh = adminPkh1
        }
    void $ Emulator.waitNSlots 2




