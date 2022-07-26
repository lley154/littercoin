{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main
    ( main
    ) where
        
import           Littercoin.OffChain       
import           Control.Monad                          (void)
import           Control.Monad.Freer                    (interpret)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Default                           (def)
import           Ledger.Address                         (PaymentPubKeyHash)
import qualified Ledger.CardanoWallet as CW
import           PabContract                            (Contracts(..))
import           Plutus.PAB.Effects.Contract.Builtin    (Builtin, BuiltinHandler(contractHandler), handleBuiltin)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import           Wallet.Emulator.Wallet                 (knownWallet)


adminPkh1 :: PaymentPubKeyHash
adminPkh1 = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 1)

adminPkh2 :: PaymentPubKeyHash
adminPkh2 = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 2)


main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do

    let w1 = knownWallet 1
        w2 = knownWallet 2
        tokenName = "Littercoin"
        qty = 50

    --setLocaleEncoding utf8
    Simulator.logString @(Builtin Contracts) "Starting PAB webserver on port 8080"
    shutdown <- PAB.Server.startServerDebug   

    Simulator.logString @(Builtin Contracts) "********* PAB server is running *********"
    Simulator.logString @(Builtin Contracts) "To start PAB simulation press return"
    void $ liftIO getLine
        
    Simulator.logString @(Builtin Contracts) "Initializing contract handle for wallet 1"
    h1 <- Simulator.activateContract w1 UseContract

    Simulator.logString @(Builtin Contracts) "Initializing contract handle for wallet 2"
    h2 <- Simulator.activateContract w2 UseContract

    Simulator.logString @(Builtin Contracts) "Calling mint endpoint for wallet 1"
    void $ Simulator.callEndpointOnInstance h1 "mint" $ TokenParams
        { tpTokenName = tokenName
        , tpQty = qty
        , tpAdminPkh = adminPkh1
        }

    Simulator.waitNSlots 2
    Simulator.logString @(Builtin Contracts) "Token minted for wallet 1, press return to continue"
    void $ liftIO getLine

    {-
    
    Simulator.logString @(Builtin Contracts) "Calling mint endpoint for wallet 2"
    void $ Simulator.callEndpointOnInstance h2 "mint" $ NFTParams
        { npAddress = address2
        , npLat = lat2
        , npLong = long2
        , npCategory = category2
        , npMethod = method2
        , npCO2Qty = cO2Qty2 
        , npAdminPkh = adminPkh2
        }
    Simulator.waitNSlots 2
    Simulator.logString @(Builtin Contracts) "Token minted for wallet 2, press return to continue"
    void $ liftIO getLine


    mpParams <- flip Simulator.waitForState h2 $ \json -> case (fromJSON json :: Result (Monoid.Last MintPolicyParams)) of
                    Success (Monoid.Last (Just mpParams))   -> Just mpParams
                    _                                       -> Nothing
      

    Simulator.logString @(Builtin Contracts) "Calling burn endpoint for wallet 2"
    void $ Simulator.callEndpointOnInstance h2 "burn" $ mpParams

    Simulator.waitNSlots 2
    Simulator.logString @(Builtin Contracts) "Token burned for wallet 2, press return to continue"
    void $ liftIO getLine

    -}

    -- Pressing enter results in the balances being printed
    Simulator.logString @(Builtin Contracts) "Balances at the end of the simulation"

    balances <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances

    shutdown


handlers :: Simulator.SimulatorEffectHandlers (Builtin Contracts)
handlers = Simulator.mkSimulatorHandlers def $ interpret (contractHandler handleBuiltin)

