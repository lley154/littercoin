{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Littercoin.OffChain where

import           Littercoin.OnChain                 (curSymbol, policy)
import           Littercoin.Types                   (MintPolicyParams(..), MintPolicyRedeemer(..))
import           Control.Monad                      (forever)
import           Data.Aeson                         (FromJSON, ToJSON)
--import qualified Data.Map as Map                    (keys)
import           Data.Monoid                        (Last (..))
import           Data.Text                          (Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import qualified Plutus.Contract as Contract        (awaitPromise, awaitTxConfirmed, Contract, Endpoint, endpoint, handleError, logError, 
                                                    logInfo, ownFirstPaymentPubKeyHash, select, submitTxConstraintsWith, tell, type (.\/), utxosAt)
import           PlutusTx                           (toBuiltinData)
import           PlutusTx.Prelude                   (Bool(..), Integer, Maybe ( Just, Nothing ), ($))
import           Ledger                             (getCardanoTxId)
import           Ledger.Constraints as Constraints  (mintingPolicy, mustMintValueWithRedeemer, mustBeSignedBy, unspentOutputs)
import           Ledger.Address as Address          (PaymentPubKeyHash(..), pubKeyHashAddress)
import           Ledger.Scripts as Scripts          (Redeemer(..))
import           Ledger.Value as Value              (singleton, TokenName(..))
import           Playground.Contract as Playground  (ToSchema)
--import           Plutus.Contract.Request as Request (ownPaymentPubKeyHash)
import           PlutusPrelude                      (void)
import qualified Prelude as Haskell                 (Semigroup (..), Show (..), String)
import           Text.Printf                        (printf)


-- | Mint Params are parameters that are used as part of the parameterized
--   minting policy script
data MintParams = MintParams
    { 
        mPTokenName   :: !TokenName
    ,   mPQty         :: !Integer
    ,   mPAdminPkh    :: !Address.PaymentPubKeyHash     
    } deriving (Generic, FromJSON, ToJSON, Haskell.Show, Playground.ToSchema)


-- | MintSchema type is defined and used by the PAB Contracts
type MintSchema = Contract.Endpoint "mint" MintParams
             Contract..\/ Contract.Endpoint "burn" MintParams



-- | mintToken is an offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator.  
mintToken :: MintParams -> Contract.Contract (Last MintPolicyParams) MintSchema Text ()
mintToken mp = do

    ownPkh <- Contract.ownFirstPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    let red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
            {
                mPRPolarity = True  -- mint token
            }
        mintPolicyParams = MintPolicyParams 
            {
                mPPTokenName = mPTokenName mp
            ,   mPPAdminPkh = mPAdminPkh mp
            }

    let val     = Value.singleton (curSymbol mintPolicyParams) (mPTokenName mp) (mPQty mp)
        lookups = Constraints.mintingPolicy (policy mintPolicyParams) Haskell.<> 
                    Constraints.unspentOutputs utxos
        tx      = Constraints.mustMintValueWithRedeemer red val Haskell.<> 
                    -- Constraints.mustSpendPubKeyOutput oref Haskell.<>
                    Constraints.mustBeSignedBy ownPkh
    ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
    void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @Haskell.String $ printf "mintToken: Forged %s" (Haskell.show val)
    Contract.logInfo @Haskell.String $ printf "mintToken: Mint params %s" (Haskell.show mintPolicyParams)

    Contract.tell $ Last $ Just mintPolicyParams


-- | burnToken is an offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
burnToken :: MintParams -> Contract.Contract w MintSchema Text ()
burnToken mp = do
    
    ownPkh <- Contract.ownFirstPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)

    let red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
            {
                mPRPolarity = False -- burn token
            }
        mintPolicyParams = MintPolicyParams 
            {
                mPPTokenName = mPTokenName mp
            ,   mPPAdminPkh = mPAdminPkh mp
            }
    let val     = Value.singleton (curSymbol mintPolicyParams) (mPTokenName mp) (mPQty mp)
        lookups = Constraints.mintingPolicy (policy mintPolicyParams) Haskell.<> 
                  Constraints.unspentOutputs utxos
        tx      = Constraints.mustMintValueWithRedeemer red val 
    ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
    void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @Haskell.String $ printf "burnToken: Burned %s" (Haskell.show val)
    Contract.logInfo @Haskell.String $ printf "burnToken: Burning params %s" (Haskell.show mintPolicyParams)




-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
endpoints :: Contract.Contract (Last MintPolicyParams) MintSchema Text ()
endpoints = forever $ Contract.handleError Contract.logError $ Contract.awaitPromise $ mint `Contract.select` burn
  where
    mint = Contract.endpoint @"mint" $ \(mp) -> mintToken mp
    burn = Contract.endpoint @"burn" $ \(mp) -> burnToken mp 

