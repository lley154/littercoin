{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main
    ( main
    ) where
        
import           Littercoin.OffChain       
import           Control.Monad                          (void)
import           Control.Monad.Freer                    (interpret)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Aeson                             (Result (..), fromJSON)
import           Data.Default                           (def)
import qualified Data.Monoid as Monoid
import           Ledger.Address                         (PaymentPubKeyHash)
import qualified Ledger.CardanoWallet as CW
import           Ledger.Value as Value                  (TokenName(..))
import           PabContract                            (Contracts(..))
import           Plutus.PAB.Effects.Contract.Builtin    (Builtin, BuiltinHandler(contractHandler), handleBuiltin)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import           Wallet.Emulator.Wallet                 (knownWallet)


adminPkh1 :: PaymentPubKeyHash
adminPkh1 = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 1)

threadTokenName :: Value.TokenName
threadTokenName = ""

ownerTokenName :: Value.TokenName
ownerTokenName = ""



main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do

    let w1 = knownWallet 1
        w2 = knownWallet 2
        tokenName = "Littercoin"
        merchantTokenName = "Littercoin Approved Merchant"
        qty1 = 100 :: Integer  -- amount of littercoin
        qty2 = 25 :: Integer -- amount of littercoin
        adaAmount = 10000000 -- 10 Ada

    --setLocaleEncoding utf8
    Simulator.logString @(Builtin Contracts) "Starting PAB webserver on port 8080"
    shutdown <- PAB.Server.startServerDebug   

    Simulator.logString @(Builtin Contracts) "********* PAB server is running *********"
    Simulator.logString @(Builtin Contracts) "To start PAB simulation press return"
    void $ liftIO getLine

    Simulator.logString @(Builtin Contracts) "Initializing Littercoin validator contract handle for wallet 1"
    initHandle <- Simulator.activateContract w1 InitContract

    void $ Simulator.callEndpointOnInstance initHandle "init" $ TokenParams
        { tpLCTokenName = tokenName
        , tpMerchantTokenName = merchantTokenName
        , tpQty = qty1  -- ignored for init
        , tpAdminPkh = adminPkh1
        , tpThreadTokenName = threadTokenName
        , tpOwnerTokenName = ownerTokenName
        }
    Simulator.waitNSlots 2

    (ttTokenName, otTokenName) <- flip Simulator.waitForState initHandle $ \json -> case (fromJSON json :: Result (Monoid.Last (Value.TokenName, Value.TokenName))) of
                    Success (Monoid.Last (Just (ttTokenName, otTokenName))) -> Just (ttTokenName, otTokenName)
                    _                                        -> Nothing
    Simulator.logString @(Builtin Contracts) $ "ThreadToken Token Name found: " ++ show ttTokenName
    Simulator.logString @(Builtin Contracts) $ "OwnerToken Token Name found: " ++ show otTokenName
    
    balances_init <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_init

    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine

        
    Simulator.logString @(Builtin Contracts) "Initializing contract handle for wallet 1"
    h1 <- Simulator.activateContract w1 UseContract

    Simulator.logString @(Builtin Contracts) "Initializing contract handle for wallet 2"
    h2 <- Simulator.activateContract w2 UseContract

    -- Add some add to the Ada to the Littercoin smart contract by a donnor
    Simulator.logString @(Builtin Contracts) "Calling addAdaContract endpoint for wallet 2"
    void $ Simulator.callEndpointOnInstance h1 "addAdaContract" $ TokenParams
        { tpLCTokenName = tokenName
        , tpMerchantTokenName = merchantTokenName
        , tpQty = adaAmount
        , tpAdminPkh = adminPkh1 
        , tpThreadTokenName = ttTokenName
        , tpOwnerTokenName = otTokenName
        }

    Simulator.waitNSlots 2

    balances_add <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_add

    Simulator.logString @(Builtin Contracts) "Ada added to Littercoin contract, press return to continue"
    void $ liftIO getLine


    -- Add some Ada to the Littercoin smart contract by a donnor
    Simulator.logString @(Builtin Contracts) "Calling addAdaContract endpoint for wallet 2"
    void $ Simulator.callEndpointOnInstance h1 "addAdaContract" $ TokenParams
        { tpLCTokenName = tokenName
        , tpMerchantTokenName = merchantTokenName
        , tpQty = adaAmount
        , tpAdminPkh = adminPkh1 
        , tpThreadTokenName = ttTokenName
        , tpOwnerTokenName = otTokenName
        }

    Simulator.waitNSlots 2

    balances_add2 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_add2

    Simulator.logString @(Builtin Contracts) "Ada added to Littercoin contract, press return to continue"
    void $ liftIO getLine


    -- Mint some littercoins for wallet 1
    Simulator.logString @(Builtin Contracts) "Calling mint endpoint for wallet 1"
    void $ Simulator.callEndpointOnInstance h1 "mintLC" $ TokenParams
        { tpLCTokenName = tokenName
        , tpMerchantTokenName = merchantTokenName
        , tpQty = qty1
        , tpAdminPkh = adminPkh1 
        , tpThreadTokenName = ttTokenName
        , tpOwnerTokenName = otTokenName
        }

    Simulator.waitNSlots 5

    balances_mint <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_mint
    
    Simulator.logString @(Builtin Contracts) "Token minted for wallet 1, press return to continue"
    void $ liftIO getLine


    -- Try minting some littercoins with incorrect pkh
    Simulator.logString @(Builtin Contracts) "Calling mint endpoint for wallet 2, but does not have adminPkh"
    void $ Simulator.callEndpointOnInstance h2 "mintLC" $ TokenParams
        { tpLCTokenName = tokenName
        , tpMerchantTokenName = merchantTokenName
        , tpQty = qty1
        , tpAdminPkh = adminPkh1 
        , tpThreadTokenName = ttTokenName
        , tpOwnerTokenName = otTokenName
        }

    Simulator.waitNSlots 2

    balances_no_pkh <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_no_pkh
    
    Simulator.logString @(Builtin Contracts) "Token minted for wallet 2?, press return to continue"
    void $ liftIO getLine
    
    -- Burn Littercoin but merchant does not have approved Merchant
    Simulator.logString @(Builtin Contracts) "Calling burn endpoint for wallet 1, but does not have merchant Merchant"
    void $ Simulator.callEndpointOnInstance h1 "burnLC" $ TokenParams
        { tpLCTokenName = tokenName
        , tpMerchantTokenName = merchantTokenName
        , tpQty = qty2
        , tpAdminPkh = adminPkh1 
        , tpThreadTokenName = ttTokenName
        , tpOwnerTokenName = otTokenName
        }

    Simulator.waitNSlots 2

    balances_no_burn <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_no_burn
    
    Simulator.logString @(Builtin Contracts) "Token burned for wallet 1? Press return to continue"
    void $ liftIO getLine


    -- Mint the merchant Merchant to allow for burning of littercoin
    Simulator.logString @(Builtin Contracts) "Calling mintMerchToken endpoint for wallet 1"
    void $ Simulator.callEndpointOnInstance h1 "mintMerchToken" $ TokenParams
        { tpLCTokenName = tokenName
        , tpMerchantTokenName = merchantTokenName
        , tpQty = qty1  -- ignored for merchant token
        , tpAdminPkh = adminPkh1 
        , tpThreadTokenName = ttTokenName
        , tpOwnerTokenName = otTokenName
        }

    Simulator.waitNSlots 5    

    balances_merchant <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_merchant

    Simulator.logString @(Builtin Contracts) "Merchant minted for wallet 1, press return to continue"
    void $ liftIO getLine


    -- Burn Littercoin with merchant approved Merchant
    Simulator.logString @(Builtin Contracts) "Calling burn endpoint for wallet 1"
    void $ Simulator.callEndpointOnInstance h1 "burnLC" $ TokenParams
        { tpLCTokenName = tokenName
        , tpMerchantTokenName = merchantTokenName
        , tpQty = qty2
        , tpAdminPkh = adminPkh1 
        , tpThreadTokenName = ttTokenName
        , tpOwnerTokenName = otTokenName
        }

    Simulator.waitNSlots 5

    balances_burn <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_burn

    Simulator.logString @(Builtin Contracts) "Token burned for wallet 1, press return to continue"
    void $ liftIO getLine



    {-
    -- Burn the merchant Merchant to remove it
    Simulator.logString @(Builtin Contracts) "Calling burn endpoint for wallet 2"
    void $ Simulator.callEndpointOnInstance h1 "burnMerchant" $ TokenParams
        { tpLCTokenName = tokenName
        , tpMerchantTokenName = merchantTokenName
        , tpQty = adaAmount
        , tpAdminPkh = adminPkh1 
        , tpThreadTokenName = ttTokenName
        , tpOwnerTokenName = otTokenName
        }

    Simulator.waitNSlots 5
    Simulator.logString @(Builtin Contracts) "Merchant burned for wallet 2, press return to continue"
    void $ liftIO getLine
    
    -- Pressing enter results in the balances being printed
    Simulator.logString @(Builtin Contracts) "Balances at the end of the simulation"

    balances3 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances3
    
    -}

    shutdown


handlers :: Simulator.SimulatorEffectHandlers (Builtin Contracts)
handlers = Simulator.mkSimulatorHandlers def $ interpret (contractHandler handleBuiltin)

