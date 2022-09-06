{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}



module Littercoin.OffChain where

import           Littercoin.OnChain                 (intToBBS, lcCurSymbol, lcHash, lcPolicy, LCDatum(..), lcValidator, minAda, nftCurSymbol, nftPolicy, nftTokenValue, threadTokenCurSymbol, threadTokenPolicy, threadTokenValue, typedLCValidator)
import           Littercoin.Types                   (LCMintPolicyParams(..), LCRedeemer(..), LCValidatorParams(..), NFTMintPolicyParams(..), MintPolicyRedeemer(..), ThreadTokenRedeemer(..))
import           Control.Lens                       (review)
import           Control.Monad                      (forever)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Map as Map                    (singleton, toList, keys)
import           Data.Monoid                        (Last (..))
import           Data.Text as T                     (Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import qualified Plutus.Contract as Contract        (AsContractError (_ConstraintResolutionContractError), awaitPromise, awaitTxConfirmed, Contract, Endpoint, endpoint, handleError, logError, 
                                                    logInfo, mapError, select, submitTxConstraintsWith, tell, throwError, type (.\/), utxosAt)
import           PlutusTx                           (fromBuiltinData, toBuiltinData)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, Integer, Maybe (..), ($))
import           Ledger                             (getCardanoTxId)
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import           Ledger.Address as Address          (PaymentPubKeyHash(..), pubKeyHashAddress, scriptHashAddress)
import           Ledger.Constraints as Constraints  (adjustUnbalancedTx, mintingPolicy, mustMintValueWithRedeemer, mustBeSignedBy, mustPayToPubKey, mustPayToTheScript, mustSpendPubKeyOutput, mustSpendScriptOutput, otherScript, typedValidatorLookups, unspentOutputs)
import           Ledger.Scripts as Scripts          (Datum(..), Redeemer(..))
import qualified Ledger.Tx as Tx                    (ChainIndexTxOut (_ciTxOutValue,_ciTxOutDatum), TxOutRef(..))
import           Ledger.TxId as TxId                (TxId(..))  
import           Ledger.Value as Value              (CurrencySymbol, singleton, split, TokenName(..), valueOf)
import           Playground.Contract as Playground  (ToSchema)
import           Plutus.Contract.Request as Request (mkTxContract, submitTxConfirmed, ownPaymentPubKeyHash)
import           Plutus.Contract.Wallet as Wallet   (getUnspentOutput)
import           PlutusPrelude                      (void)
import           PlutusTx.Prelude                   (sha2_256, (+), (++), (==))
import qualified Prelude as Haskell                 (Either(..), return, Semigroup ((<>)), Show (..), String)
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
                pkh = tpAdminPkh tp
                nftMintParams = NFTMintPolicyParams
                    {
                        nftTokenName = tn' -- the name of the NFT token
                    ,   nftAdminPkh = pkh
                    }
                (_, nftTokVal) = Value.split(nftTokenValue (nftCurSymbol nftMintParams) tn')
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     }
                mintParams = LCMintPolicyParams 
                    {
                        lcTokenName = tn
                    ,   lcAdminPkh = pkh
                    ,   lcNFTTokenValue = nftTokVal
                    }
            let val     = Value.singleton (lcCurSymbol mintParams) tn (-(tpQty tp))
                lookups = Constraints.mintingPolicy (lcPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val Haskell.<>
                          Constraints.mustPayToPubKey pkh (minAda Haskell.<> nftTokVal)
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


-- | Initialize the littercoin contract
initLCValidator :: TokenParams -> Contract.Contract (Last Value.TokenName) s T.Text ()
initLCValidator tp = do
    txOutRef <- Wallet.getUnspentOutput
    let txBS = TxId.getTxId(Tx.txOutRefId txOutRef) Haskell.<> intToBBS(Tx.txOutRefIdx txOutRef) 
        threadTokenName  = Value.TokenName $ sha2_256 txBS
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol threadTokenName)

    Contract.logInfo $ "initLCValidator: thread token name= " ++ Haskell.show threadTokenName

    ownPkh <- ownPaymentPubKeyHash
    utxo <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)

    let tn = Value.TokenName $ tpLCTokenName tp
        tn' = Value.TokenName $ tpNFTTokenName tp
        nftMintParams = NFTMintPolicyParams
            {
                nftTokenName = tn' -- the name of the NFT token
            ,   nftAdminPkh = tpAdminPkh tp
            }
        (_, nftTokVal) = Value.split(nftTokenValue (nftCurSymbol nftMintParams) tn')
        --red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ AddAda (tpQty tp)
        lcParams = LCValidatorParams
            {   lcvAdminPkh         = tpAdminPkh tp
            ,   lcvNFTTokenValue    = nftTokVal
            ,   lcvLCTokenName      = tn
            }
        lcDatum = LCDatum 
            {   adaAmount = 0                                         
            ,   lcAmount = 0
            }

        red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ ThreadTokenRedeemer txOutRef
        dat = PlutusTx.toBuiltinData lcDatum
        lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams)
            Haskell.<> Constraints.mintingPolicy threadTokenPolicy 
            Haskell.<> Constraints.unspentOutputs utxo
        tx = Constraints.mustPayToTheScript dat (minAda Haskell.<> ttVal)
            Haskell.<> Constraints.mustMintValueWithRedeemer red ttVal
            Haskell.<> Constraints.mustSpendPubKeyOutput txOutRef

    utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    Request.submitTxConfirmed adjustedUtx

    --ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
    --void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo $ "initLotto: tx submitted successfully= " ++ Haskell.show adjustedUtx
    --Contract.logInfo $ "initLotto: tx submitted successfully" 

    Contract.tell $ Last $ Just threadTokenName


-- | Find the littercoin validator onchain using the lc params and threadtoken
findLCValidator :: LCValidatorParams -> Value.CurrencySymbol -> Value.TokenName -> Contract.Contract w s T.Text (Tx.TxOutRef, Tx.ChainIndexTxOut, LCDatum)
findLCValidator params cs tn = do
    utxos <- Contract.utxosAt $ Address.scriptHashAddress $ lcHash $ PlutusTx.toBuiltinData params
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (Tx._ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case Tx._ciTxOutDatum o of
            Haskell.Left _          -> Contract.throwError "findLCValidator: datum missing"
            Haskell.Right (Scripts.Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> Contract.throwError "findLCValidator: datum has wrong type"
                Just d@LCDatum{} -> Haskell.return (oref, o, d)
        _           -> Contract.throwError "findLCValidator: utxo not found"


-- | Add Ada to the littercoin smart contract  This offchain function is only used by the PAB
--   simulator to test the validation rules of littercoin validator. 
--addAdaContract :: Value.TokenName -> TokenParams -> Contract.Contract () TokenSchema Text ()
addAdaToContract :: Value.TokenName -> TokenParams -> Contract.Contract w s T.Text ()
addAdaToContract tt tp = do

    let tn = Value.TokenName $ tpLCTokenName tp
        tn' = Value.TokenName $ tpNFTTokenName tp
        nftMintParams = NFTMintPolicyParams
            {
                nftTokenName = tn' -- the name of the NFT token
            ,   nftAdminPkh = tpAdminPkh tp
            }
        (_, nftTokVal) = Value.split(nftTokenValue (nftCurSymbol nftMintParams) tn')
        lcParams = LCValidatorParams
            {   lcvAdminPkh         = tpAdminPkh tp
            ,   lcvNFTTokenValue    = nftTokVal
            ,   lcvLCTokenName      = tn
            }
        
    (oref, o, lcd@LCDatum{}) <- findLCValidator lcParams threadTokenCurSymbol tt
    Contract.logInfo $ "addAdaContract: found littercoin utxo with datum= " ++ Haskell.show lcd
    Contract.logInfo $ "addAdaContract: found littercoin utxo oref= " ++ Haskell.show oref
    Contract.logInfo $ "addAdaContract: lotto littercoin hash= " ++ Haskell.show (lcHash $ PlutusTx.toBuiltinData lcParams)

    let lcDatum = LCDatum
            {   adaAmount = (adaAmount lcd) + (tpQty tp)                                                  
            ,   lcAmount = lcAmount lcd
            }
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol tt)
        red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ AddAda (tpQty tp)
        dat = PlutusTx.toBuiltinData lcDatum

        lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams)
            Haskell.<> Constraints.otherScript (lcValidator $ PlutusTx.toBuiltinData lcParams)
            Haskell.<> Constraints.unspentOutputs (Map.singleton oref o)
        tx = Constraints.mustPayToTheScript dat (Ada.lovelaceValueOf (tpQty tp) Haskell.<> ttVal)
            Haskell.<> Constraints.mustSpendScriptOutput oref red

    utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    Request.submitTxConfirmed adjustedUtx

    --ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
    --void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx

    --Contract.logInfo $ "addAdaContract: tx submitted"
    Contract.logInfo $ "addAdaContract: tx submitted successfully= " ++ Haskell.show adjustedUtx

-- | InitSchema type is defined and used by the PAB Contracts
type InitSchema =
        Contract.Endpoint "init" TokenParams


-- | TokenSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mintLC" TokenParams
                   Contract..\/ Contract.Endpoint "addAdaContract" (Value.TokenName, TokenParams)
                   Contract..\/ Contract.Endpoint "burnLC" TokenParams
                   Contract..\/ Contract.Endpoint "mintNFT" TokenParams
                   Contract..\/ Contract.Endpoint "burnNFT" TokenParams
    

-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
initEndpoint :: Contract.Contract (Last Value.TokenName) InitSchema T.Text ()
initEndpoint = forever
              $ Contract.handleError Contract.logError
              $ Contract.awaitPromise
              $ Contract.endpoint @"init" $ \tp -> initLCValidator tp


-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
useEndpoints :: Contract.Contract () TokenSchema Text ()
useEndpoints = forever $ Contract.handleError Contract.logError $ Contract.awaitPromise $ 
                addAdaContract `Contract.select`
                mintLC `Contract.select` 
                burnLC `Contract.select` 
                mintNFT `Contract.select` 
                burnNFT 
               
    where
        addAdaContract = Contract.endpoint @"addAdaContract" $ \(tt, tp) -> addAdaToContract tt tp 
        mintLC = Contract.endpoint @"mintLC" $ \(tp) -> mintLCToken tp
        burnLC = Contract.endpoint @"burnLC" $ \(tp) -> burnLCToken tp 
        mintNFT = Contract.endpoint @"mintNFT" $ \(tp) -> mintNFTToken tp
        burnNFT = Contract.endpoint @"burnNFT" $ \(tp) -> burnNFTToken tp



