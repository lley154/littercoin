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

--adminPkh2 :: PaymentPubKeyHash
--adminPkh2 = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 2)


main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do

    let w1 = knownWallet 1
        w2 = knownWallet 2
        tokenName = "Littercoin"
        nftTokenName = "Littercoin Approved Merchant"
        qty1 = 100
        qty2 = 25

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


    -- Mint some littercoins for wallet 1
    Simulator.logString @(Builtin Contracts) "Calling mint endpoint for wallet 1"
    void $ Simulator.callEndpointOnInstance h1 "mintLC" $ TokenParams
        { tpLCTokenName = tokenName
        , tpNFTTokenName = nftTokenName
        , tpQty = qty1
        , tpAdminPkh = adminPkh1
        }

    Simulator.waitNSlots 2
    Simulator.logString @(Builtin Contracts) "Token minted for wallet 1, press return to continue"
    void $ liftIO getLine

    -- Try minting some littercoins with incorrect pkh
    Simulator.logString @(Builtin Contracts) "Calling mint endpoint for wallet 2, but does not have adminPkh"
    void $ Simulator.callEndpointOnInstance h2 "mintLC" $ TokenParams
        { tpLCTokenName = tokenName
        , tpNFTTokenName = nftTokenName
        , tpQty = qty1
        , tpAdminPkh = adminPkh1
        }

    Simulator.waitNSlots 2
    Simulator.logString @(Builtin Contracts) "Token minted for wallet 2? press return to continue"
    void $ liftIO getLine
    
    -- Burn official Littercoin but merchant does not have approved NFT
    Simulator.logString @(Builtin Contracts) "Calling burn endpoint for wallet 1"
    void $ Simulator.callEndpointOnInstance h1 "burnLC" $ TokenParams
        { tpLCTokenName = tokenName
        , tpNFTTokenName = nftTokenName
        , tpQty = qty2
        , tpAdminPkh = adminPkh1 -- ignored for littercoin burning
        }

    Simulator.waitNSlots 2
    Simulator.logString @(Builtin Contracts) "Token burned for wallet 1? press return to continue"
    void $ liftIO getLine


    -- Mint the merchant NFT to allow for burning of littercoin
    Simulator.logString @(Builtin Contracts) "Calling mint endpoint for wallet 2"
    void $ Simulator.callEndpointOnInstance h1 "mintNFT" $ TokenParams
        { tpLCTokenName = tokenName -- ignored for NFT minting
        , tpNFTTokenName = nftTokenName
        , tpQty = 1
        , tpAdminPkh = adminPkh1
        }

    Simulator.waitNSlots 2
    Simulator.logString @(Builtin Contracts) "NFT minted for wallet 2, press return to continue"
    void $ liftIO getLine


    -- Burn Littercoin with merchant approved NFT
    Simulator.logString @(Builtin Contracts) "Calling burn endpoint for wallet 1"
    void $ Simulator.callEndpointOnInstance h1 "burnLC" $ TokenParams
        { tpLCTokenName = tokenName
        , tpNFTTokenName = nftTokenName
        , tpQty = qty2
        , tpAdminPkh = adminPkh1 -- ignored for littercoin burning
        }

    Simulator.waitNSlots 2
    Simulator.logString @(Builtin Contracts) "Token burned for wallet 1, press return to continue"
    void $ liftIO getLine
    

    {-
    -- Burn the merchant NFT to allow for burning of littercoin
    Simulator.logString @(Builtin Contracts) "Calling burn endpoint for wallet 2"
    void $ Simulator.callEndpointOnInstance h1 "burnNFT" $ TokenParams
        { tpLCTokenName = tokenName -- ignored for NFT minting
        , tpNFTTokenName = nftTokenName
        , tpQty = 1
        , tpAdminPkh = adminPkh1 -- ignored for NFT burning
        }

    Simulator.waitNSlots 2
    Simulator.logString @(Builtin Contracts) "NFT burned for wallet 2, press return to continue"
    void $ liftIO getLine
    -}


    -- Pressing enter results in the balances being printed
    Simulator.logString @(Builtin Contracts) "Balances at the end of the simulation"

    balances <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances

    shutdown


handlers :: Simulator.SimulatorEffectHandlers (Builtin Contracts)
handlers = Simulator.mkSimulatorHandlers def $ interpret (contractHandler handleBuiltin)

