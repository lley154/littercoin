{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Littercoin.OffChain where

import           Littercoin.OnChain                (curSymbol, policy)
import           Littercoin.Types                  (MintPolicyParams(..), MintPolicyRedeemer(..))
import           Littercoin.Utils                  (integerToBS)
import           Control.Monad                      (forever)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Map as Map                    (keys)
import           Data.Monoid                        (Last (..))
import           Data.Text                          (Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import qualified Plutus.Contract as Contract        (awaitPromise, awaitTxConfirmed, Contract, Endpoint, endpoint, handleError, logError, 
                                                    logInfo, select, submitTxConstraintsWith, tell, type (.\/), utxosAt)
import           PlutusTx                           (toBuiltinData)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, Integer, Maybe ( Just, Nothing ), sha2_256, ($))
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


-- | NFT Params are parameters that are used as part of the parameterized
--   minting policy script
data NFTParams = NFTParams
    { 
      npAddress     :: !BuiltinByteString
    , npLat         :: !Integer
    , npLong        :: !Integer
    , npCategory    :: !BuiltinByteString
    , npMethod      :: !BuiltinByteString
    , npCO2Qty      :: !Integer
    , npAdminPkh    :: !Address.PaymentPubKeyHash     
    } deriving (Generic, FromJSON, ToJSON, Haskell.Show, Playground.ToSchema)


-- | mintToken mints a carbon credit token.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator.   The
--   actual NFT and metadata creation is done via the cardano-cli in the scripts
--   directory.
mintToken :: NFTParams -> Contract.Contract (Last MintPolicyParams) NFTSchema Text ()
mintToken np = do

    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let tn = (npAddress np) Haskell.<> 
                     (integerToBS $ npLat np) Haskell.<> 
                     (integerToBS $ npLong np) Haskell.<> 
                     (npCategory np) Haskell.<> 
                     (npMethod np) Haskell.<> 
                     (integerToBS $ npCO2Qty np)
                tn' = Value.TokenName $ sha2_256 tn
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     }
                mintParams = MintPolicyParams 
                    {
                      mpOref = oref
                    , mpTokenName = tn'
                    , mpAddress = npAddress np
                    , mpLat = npLat np
                    , mpLong = npLong np
                    , mpCategory = npCategory np
                    , mpMethod = npMethod np
                    , mpCO2Qty = npCO2Qty np
                    , mpAdminPkh = npAdminPkh np
                    }

            let val     = Value.singleton (curSymbol mintParams) tn' 1
                lookups = Constraints.mintingPolicy (policy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val Haskell.<> 
                          Constraints.mustSpendPubKeyOutput oref Haskell.<>
                          Constraints.mustBeSignedBy ownPkh
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "mintToken: Forged %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "mintToken: NFT params %s" (Haskell.show mintParams)

            Contract.tell $ Last $ Just mintParams


-- | burnToken burns a carbon credit token.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator.   The
--   actual NFT buring is done via the cardano-cli in the scripts directory.
burnToken :: MintPolicyParams -> Contract.Contract w NFTSchema Text ()
burnToken mpParams = do
    
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "burnToken: No utxo found"
        oref : _ -> do
            let tn = mpTokenName mpParams
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     }
            let val     = Value.singleton (curSymbol mpParams) tn (-1)
                lookups = Constraints.mintingPolicy (policy mpParams) Haskell.<> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val Haskell.<> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "burnToken: Burned %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "burnToken: Burning params %s" (Haskell.show mpParams)


-- | NFTSchema type is defined and used by the PAB Contracts
type NFTSchema = Contract.Endpoint "mint" NFTParams
             Contract..\/ Contract.Endpoint "burn" MintPolicyParams


-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
endpoints :: Contract.Contract (Last MintPolicyParams) NFTSchema Text ()
endpoints = forever $ Contract.handleError Contract.logError $ Contract.awaitPromise $ mint `Contract.select` burn
  where
    mint = Contract.endpoint @"mint" $ \(np) -> mintToken np
    burn = Contract.endpoint @"burn" $ \(mp) -> burnToken mp 


