{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Littercoin.OffChain where

import           Littercoin.OnChain                 (lcCurSymbol, lcPolicy, nftCurSymbol, nftPolicy, nftTokenValue)
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
import           Ledger.Value as Value              (singleton, split, TokenName(..))
import           Playground.Contract as Playground  (ToSchema)
import           Plutus.Contract.Request as Request (ownPaymentPubKeyHash)
import           PlutusPrelude                      (void)
import qualified Prelude as Haskell                 (Semigroup (..), Show (..), String)
import           Text.Printf                        (printf)


-- | TokenParams are parameters that are used as part of the parameterized
--   minting policy script
data TokenParams = TokenParams
    { 
      tpLCTokenName         :: !BuiltinByteString
    , tpNFTTokenName        :: !BuiltinByteString  
    , tpQty                 :: !Integer
    , tpAdminPkh            :: !Address.PaymentPubKeyHash     
    } deriving (Generic, FromJSON, ToJSON, Haskell.Show, Playground.ToSchema)


-- | mintLC mints littercoin tokens.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintLCToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
mintLCToken tp = do

    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let tn = Value.TokenName $ tpLCTokenName tp
                tn' = Value.TokenName $ tpNFTTokenName tp
                nftMintParams = NFTMintPolicyParams
                    {
                        nftTokenName = tn' -- the name of the NFT token
                    ,   nftAdminPkh = tpAdminPkh tp
                    }
                (_, nftTokVal) = Value.split(nftTokenValue (nftCurSymbol nftMintParams) tn')
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     }
                mintParams = LCMintPolicyParams 
                    {
                        lcTokenName = tn -- the name of the littercoin
                    ,   lcAdminPkh = tpAdminPkh tp  -- the admin pkh who can only mint littercoins
                    ,   lcNFTTokenValue = nftTokVal  -- this contains the NFT that merchants used for burning
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
burnLCToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
burnLCToken tp = do
    
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "burnToken: No utxo found"
        _ : _ -> do
            let tn = Value.TokenName $ tpLCTokenName tp
                tn' = Value.TokenName $ tpNFTTokenName tp
                nftMintParams = NFTMintPolicyParams
                    {
                        nftTokenName = tn' -- the name of the NFT token
                    ,   nftAdminPkh = tpAdminPkh tp
                    }
                (_, nftTokVal) = Value.split(nftTokenValue (nftCurSymbol nftMintParams) tn')
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     }
                mintParams = LCMintPolicyParams 
                    {
                        lcTokenName = tn
                    ,   lcAdminPkh = tpAdminPkh tp
                    ,   lcNFTTokenValue = nftTokVal
                    }
            let val     = Value.singleton (lcCurSymbol mintParams) tn (-(tpQty tp))
                lookups = Constraints.mintingPolicy (lcPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val 
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "burnToken: Burned %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "burnToken: Burning params %s" (Haskell.show mintParams)


-- | mintNFT mints the merchant approved NFT.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintNFTToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
mintNFTToken tp = do

    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let tn = Value.TokenName $ tpNFTTokenName tp
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     }
                mintParams = NFTMintPolicyParams 
                    {
                        nftTokenName = tn -- the name of the littercoin
                    ,   nftAdminPkh = tpAdminPkh tp  -- the admin pkh who can only mint merch NFTs
                    }

            let val     = Value.singleton (nftCurSymbol mintParams) tn (tpQty tp)
                lookups = Constraints.mintingPolicy (nftPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val Haskell.<> 
                          Constraints.mustSpendPubKeyOutput oref Haskell.<>
                          Constraints.mustBeSignedBy ownPkh
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "mintNFT: Forged %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "mintNFT: Token params %s" (Haskell.show mintParams)



-- | burnNFT burns the merchant approved NFT.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator.  
burnNFTToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
burnNFTToken tp = do
    
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "burnToken: No utxo found"
        _ : _ -> do
            let tn = Value.TokenName $ tpNFTTokenName tp
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     }
                mintParams = NFTMintPolicyParams 
                    {
                        nftTokenName = tn
                    ,   nftAdminPkh = tpAdminPkh tp
                    }
            let val     = Value.singleton (nftCurSymbol mintParams) tn (-(tpQty tp))
                lookups = Constraints.mintingPolicy (nftPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val 
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "burnToken: Burned %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "burnToken: Burning params %s" (Haskell.show mintParams)


-- | NFTSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mintLC" TokenParams
                   Contract..\/ Contract.Endpoint "burnLC" TokenParams
                   Contract..\/ Contract.Endpoint "mintNFT" TokenParams
                   Contract..\/ Contract.Endpoint "burnNFT" TokenParams


-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
endpoints :: Contract.Contract () TokenSchema Text ()
endpoints = forever $ Contract.handleError Contract.logError $ Contract.awaitPromise $ 
                mintLC `Contract.select` 
                burnLC `Contract.select` 
                mintNFT `Contract.select` 
                burnNFT 

  where
    mintLC = Contract.endpoint @"mintLC" $ \(tp) -> mintLCToken tp
    burnLC = Contract.endpoint @"burnLC" $ \(tp) -> burnLCToken tp 
    mintNFT = Contract.endpoint @"mintNFT" $ \(tp) -> mintNFTToken tp
    burnNFT = Contract.endpoint @"burnNFT" $ \(tp) -> burnNFTToken tp 


