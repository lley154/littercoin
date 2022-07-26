{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Littercoin.OffChain where

import           Littercoin.OnChain                 (lcCurSymbol, lcPolicy, nftCurSymbol, nftPolicy)
import           Littercoin.Types                   (LCMintPolicyParams(..), NFTMintPolicyParams(..), MintPolicyRedeemer(..))
import           Control.Monad                      (forever)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Map as Map                    (keys)
import           Data.Text                          (Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import qualified Plutus.Contract as Contract        (awaitPromise, awaitTxConfirmed, Contract, Endpoint, endpoint, handleError, logError, 
                                                    logInfo, select, submitTxConstraintsWith, type (.\/), utxosAt)
import           PlutusTx                           (toBuiltinData)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, Integer, Maybe ( Nothing ), ($))
import           Ledger                             (getCardanoTxId)
import           Ledger.Constraints as Constraints  (mintingPolicy, mustMintValueWithRedeemer, mustBeSignedBy, mustSpendPubKeyOutput, unspentOutputs)
import           Ledger.Address as Address          (PaymentPubKeyHash(..), pubKeyHashAddress)
import           Ledger.Scripts as Scripts          (Redeemer(..))
import           Ledger.Value as Value              (singleton, TokenName(..))
import           Playground.Contract as Playground  (ToSchema)
import           Plutus.Contract.Request as Request (ownPaymentPubKeyHash)
import           PlutusPrelude                      (void)
import qualified Prelude as Haskell                 (Semigroup (..), Show (..), String)
import           Text.Printf                        (printf)


-- | TokenParams are parameters that are used as part of the parameterized
--   minting policy script
data TokenParams = TokenParams
    { 
      tpTokenName       :: !BuiltinByteString
    , tpQty             :: !Integer
    , tpAdminPkh        :: !Address.PaymentPubKeyHash     
    } deriving (Generic, FromJSON, ToJSON, Haskell.Show, Playground.ToSchema)


-- | mintLC mints littercoin tokens.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintLC :: TokenParams -> Contract.Contract () TokenSchema Text ()
mintLC tp = do

    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let tn = Value.TokenName $ tpTokenName tp
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     }
                mintParams = LCMintPolicyParams 
                    {
                        lcTokenName = tn
                    ,   lcAdminPkh = tpAdminPkh tp
                    }

            let val     = Value.singleton (lcCurSymbol mintParams) tn (tpQty tp)
                lookups = Constraints.mintingPolicy (lcPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val Haskell.<> 
                          Constraints.mustSpendPubKeyOutput oref Haskell.<>
                          Constraints.mustBeSignedBy ownPkh
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "mintToken: Forged %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "mintToken: Token params %s" (Haskell.show mintParams)



-- | burnLC burns littercoin tokens.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator.  
burnLC :: TokenParams -> Contract.Contract () TokenSchema Text ()
burnLC tp = do
    
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "burnToken: No utxo found"
        _ : _ -> do
            let tn = Value.TokenName $ tpTokenName tp
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     }
                mintParams = LCMintPolicyParams 
                    {
                        lcTokenName = tn
                    ,   lcAdminPkh = tpAdminPkh tp
                    }
            let val     = Value.singleton (lcCurSymbol mintParams) tn (-(tpQty tp))
                lookups = Constraints.mintingPolicy (lcPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val 
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "burnToken: Burned %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "burnToken: Burning params %s" (Haskell.show mintParams)


-- | NFTSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mint" TokenParams
                   Contract..\/ Contract.Endpoint "burn" TokenParams


-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
endpoints :: Contract.Contract () TokenSchema Text ()
endpoints = forever $ Contract.handleError Contract.logError $ Contract.awaitPromise $ mint `Contract.select` burn
  where
    mint = Contract.endpoint @"mint" $ \(tp) -> mintLC tp
    burn = Contract.endpoint @"burn" $ \(tp) -> burnLC tp 


