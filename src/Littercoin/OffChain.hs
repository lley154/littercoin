{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}


module Littercoin.OffChain    
    (   
        initEndpoint
    ,   InitSchema
    ,   TokenSchema
    ,   useEndpoint
    ,   TokenParams (..)
    ) where

import           Littercoin.OnChain                 (intToBBS, lcCurSymbol, lcHash, lcPolicy, LCDatum(..), lcValidator, minAda, merchantTokenCurSymbol, merchantTokenPolicy, merchantTokenValue, threadTokenCurSymbol, threadTokenPolicy, threadTokenValue, typedLCValidator)
import           Littercoin.Types                   (LCMintPolicyParams(..), LCRedeemer(..), LCValidatorParams(..), MerchantTokenMintPolicyParams(..), MintPolicyRedeemer(..), ThreadTokenRedeemer(..))
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
import           PlutusTx.Prelude                   (divide, sha2_256, (+), (-), (++), (==), (*))
import qualified Prelude as Haskell                 (Either(..), return, Semigroup ((<>)), Show (..), String)
import           Text.Printf                        (printf)



-- | TokenParams are parameters that are passed to the endpoints
data TokenParams = TokenParams
    { 
      tpLCTokenName         :: !BuiltinByteString
    , tpMerchantTokenName   :: !BuiltinByteString  
    , tpQty                 :: !Integer
    , tpAdminPkh            :: !Address.PaymentPubKeyHash
    , tpThreadTokenName     :: !Value.TokenName
    , tpOwnerTokenName      :: !Value.TokenName
    } deriving (Generic, FromJSON, ToJSON, Haskell.Show, Playground.ToSchema)



-- | Initialize the littercoin contract
initLCValidator :: TokenParams -> Contract.Contract (Last (Value.TokenName, Value.TokenName)) s T.Text ()
initLCValidator tp = do
    txOutRef <- Wallet.getUnspentOutput
    let txBS = TxId.getTxId(Tx.txOutRefId txOutRef) Haskell.<> intToBBS(Tx.txOutRefIdx txOutRef) 
        threadTokenName  = Value.TokenName $ sha2_256 txBS
        ownerTokenName = Value.TokenName $ sha2_256 $ sha2_256 txBS
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol threadTokenName)
        (_, otVal) = Value.split(threadTokenValue threadTokenCurSymbol ownerTokenName)

    Contract.logInfo $ "initLCValidator: thread token name= " ++ Haskell.show threadTokenName

    ownPkh <- ownPaymentPubKeyHash
    utxo <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)

    let tn = Value.TokenName $ tpLCTokenName tp
        tn' = Value.TokenName $ tpMerchantTokenName tp
        merchantTokenMintParams = MerchantTokenMintPolicyParams
            {
                mtMerchantTokenName = tn' -- the name of the merchantToken token
            ,   mtAdminPkh = tpAdminPkh tp
            ,   mtOwnerTokenValue = otVal
            }
        (_, merchTokenVal) = Value.split(merchantTokenValue (merchantTokenCurSymbol merchantTokenMintParams) tn')
        --red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ AddAda (tpQty tp)
        lcParams = LCValidatorParams
            {   lcvAdminPkh = tpAdminPkh tp
            ,   lcvMerchantTokenValue = merchTokenVal
            ,   lcvLCTokenName  = tn
            ,   lcvThreadTokenValue = ttVal
            ,   lcvOwnerTokenValue = otVal
            }
        lcDatum = LCDatum 
            {   lcAdaAmount = 0                                         
            ,   lcAmount = 0
            }

        red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ ThreadTokenRedeemer txOutRef
        dat = PlutusTx.toBuiltinData lcDatum
        lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams)
            Haskell.<> Constraints.mintingPolicy threadTokenPolicy 
            Haskell.<> Constraints.unspentOutputs utxo
        tx = Constraints.mustPayToTheScript dat (minAda Haskell.<> ttVal)
            Haskell.<> Constraints.mustMintValueWithRedeemer red (ttVal Haskell.<> otVal)
            Haskell.<> Constraints.mustSpendPubKeyOutput txOutRef

    utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    Request.submitTxConfirmed adjustedUtx
    Contract.logInfo $ "initLCValidator: tx submitted successfully= " ++ Haskell.show adjustedUtx

    Contract.tell $ Last $ Just (threadTokenName, ownerTokenName)




-- | mintLC mints Littercoin tokens and increments the Littercoin counter
--   in the LC validator datum.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintLCToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
mintLCToken tp = do

    let pkh = tpAdminPkh tp
        tn = Value.TokenName $ tpLCTokenName tp
        tn' = Value.TokenName $ tpMerchantTokenName tp
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol (tpThreadTokenName tp))
        (_, otVal) = Value.split(threadTokenValue threadTokenCurSymbol (tpOwnerTokenName tp))
        merchantTokenMintParams = MerchantTokenMintPolicyParams
            {
                mtMerchantTokenName = tn' -- the name of the merchantToken Merchant token
            ,   mtAdminPkh = tpAdminPkh tp
            ,   mtOwnerTokenValue = otVal
            }
        (_, merchTokenVal) = Value.split(merchantTokenValue (merchantTokenCurSymbol merchantTokenMintParams) tn')
 
        lcParams = LCValidatorParams
            {   lcvAdminPkh = tpAdminPkh tp
            ,   lcvMerchantTokenValue  = merchTokenVal
            ,   lcvLCTokenName = tn
            ,   lcvThreadTokenValue = ttVal
            ,   lcvOwnerTokenValue = otVal
            }
        
    (orefLC, oLC, lcd@LCDatum{}) <- findLCValidator lcParams threadTokenCurSymbol (tpThreadTokenName tp)
    Contract.logInfo $ "mintLCToken: found littercoin utxo with datum= " ++ Haskell.show lcd
    Contract.logInfo $ "mintLCToken: found littercoin utxo oref= " ++ Haskell.show orefLC
    Contract.logInfo $ "mintLCToken: littercoin hash= " ++ Haskell.show (lcHash $ PlutusTx.toBuiltinData lcParams)

    let lcDatum = LCDatum
            {   lcAdaAmount = lcAdaAmount lcd                                                  
            ,   lcAmount = (lcAmount lcd) + (tpQty tp)
            }
        redLC = Scripts.Redeemer $ PlutusTx.toBuiltinData $ MintLC (tpQty tp)
        datLC = PlutusTx.toBuiltinData lcDatum

    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintLCToken: No utxo found"
        oref : _ -> do
            let red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     ,  mpTotalAdaAmount = lcAdaAmount lcd
                     ,  mpWithdrawAmount = 0 -- ignored during minting   
                     }
                mintParams = LCMintPolicyParams 
                    {
                        lcTokenName = tn -- the name of the littercoin
                    ,   lcAdminPkh = tpAdminPkh tp  -- the admin pkh who can only mint littercoins
                    ,   lcMerchantTokenValue = merchTokenVal  -- this contains the MerchantToken that merchants used for burning
                    ,   lcOwnerTokenValue = otVal
                    ,   lcThreadTokenValue = ttVal
                    }

            let val     = Value.singleton (lcCurSymbol mintParams) tn (tpQty tp)
                lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams) Haskell.<> 
                          Constraints.otherScript (lcValidator $ PlutusTx.toBuiltinData lcParams) Haskell.<> 
                          Constraints.unspentOutputs (Map.singleton orefLC oLC) Haskell.<> 
                          Constraints.mintingPolicy (lcPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustPayToTheScript datLC (Ada.lovelaceValueOf (lcAdaAmount lcd) Haskell.<> ttVal) Haskell.<> 
                          Constraints.mustSpendScriptOutput orefLC redLC Haskell.<>
                          Constraints.mustMintValueWithRedeemer red val Haskell.<> 
                          Constraints.mustSpendPubKeyOutput oref Haskell.<>
                          Constraints.mustBeSignedBy ownPkh Haskell.<>
                          Constraints.mustPayToPubKey ownPkh (minAda Haskell.<> otVal)

            utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
            let adjustedUtx = Constraints.adjustUnbalancedTx utx
            Request.submitTxConfirmed adjustedUtx
            Contract.logInfo $ "mintLCToken: tx submitted successfully= " ++ Haskell.show adjustedUtx


-- | burnLC burns littercoin tokens.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator.  
burnLCToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
burnLCToken tp = do
    
    let tn = Value.TokenName $ tpLCTokenName tp
        tn' = Value.TokenName $ tpMerchantTokenName tp
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol (tpThreadTokenName tp))
        (_, otVal) = Value.split(threadTokenValue threadTokenCurSymbol (tpOwnerTokenName tp))
        merchantTokenMintParams = MerchantTokenMintPolicyParams
            {
                mtMerchantTokenName = tn' -- the name of the merchantToken Merchant token
            ,   mtAdminPkh = tpAdminPkh tp
            ,   mtOwnerTokenValue = otVal
            }
        (_, merchTokenVal) = Value.split(merchantTokenValue (merchantTokenCurSymbol merchantTokenMintParams) tn')
        lcParams = LCValidatorParams
            {   lcvAdminPkh = tpAdminPkh tp
            ,   lcvMerchantTokenValue = merchTokenVal
            ,   lcvLCTokenName = tn
            ,   lcvThreadTokenValue = ttVal
            ,   lcvOwnerTokenValue = otVal
            }
        
    (orefLC, oLC, lcd@LCDatum{}) <- findLCValidator lcParams threadTokenCurSymbol (tpThreadTokenName tp)
    Contract.logInfo $ "burnLCToken: found littercoin utxo with datum= " ++ Haskell.show lcd
    Contract.logInfo $ "burnLCToken: found littercoin utxo oref= " ++ Haskell.show orefLC
    Contract.logInfo $ "burnLCToken: littercoin hash= " ++ Haskell.show (lcHash $ PlutusTx.toBuiltinData lcParams)

    let withdrawAda = (divide (lcAdaAmount lcd) (lcAmount lcd)) * (tpQty tp) 
        lcDatum = LCDatum
            {   lcAdaAmount = (lcAdaAmount lcd) - withdrawAda                                                
            ,   lcAmount = (lcAmount lcd) - (tpQty tp)
            }
        redLC = Scripts.Redeemer $ PlutusTx.toBuiltinData $ BurnLC (tpQty tp)
        datLC = PlutusTx.toBuiltinData lcDatum
    
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "burnLCToken: No utxo found"
        oref : _ -> do
            let pkh = tpAdminPkh tp
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     ,  mpTotalAdaAmount = (lcAdaAmount lcd) - withdrawAda
                     ,  mpWithdrawAmount = withdrawAda
                     }
                mintParams = LCMintPolicyParams 
                    {
                        lcTokenName = tn
                    ,   lcAdminPkh = pkh
                    ,   lcMerchantTokenValue = merchTokenVal
                    ,   lcOwnerTokenValue = otVal
                    ,   lcThreadTokenValue = ttVal
                    }
            let val     = Value.singleton (lcCurSymbol mintParams) tn (-(tpQty tp))
                lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams) Haskell.<> 
                          Constraints.otherScript (lcValidator $ PlutusTx.toBuiltinData lcParams) Haskell.<> 
                          Constraints.unspentOutputs (Map.singleton orefLC oLC) Haskell.<> 
                          Constraints.mintingPolicy (lcPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustPayToTheScript datLC (Ada.lovelaceValueOf ((lcAdaAmount lcd) - withdrawAda) Haskell.<> ttVal) Haskell.<> 
                          Constraints.mustSpendScriptOutput orefLC redLC Haskell.<>
                          Constraints.mustMintValueWithRedeemer red val Haskell.<>
                          Constraints.mustSpendPubKeyOutput oref Haskell.<>
                          Constraints.mustBeSignedBy ownPkh Haskell.<>
                          Constraints.mustPayToPubKey ownPkh ((Ada.lovelaceValueOf withdrawAda) Haskell.<> merchTokenVal)

            utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
            let adjustedUtx = Constraints.adjustUnbalancedTx utx
            Request.submitTxConfirmed adjustedUtx
            Contract.logInfo $ "burnLCToken: tx submitted successfully= " ++ Haskell.show adjustedUtx


-- | mintMerchantToken mints the merchant approved MerchantToken.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintMerchantToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
mintMerchantToken tp = do
     
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let tn = Value.TokenName $ tpMerchantTokenName tp
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     ,  mpTotalAdaAmount = 0 -- ignored for MerchantToken minting
                     ,  mpWithdrawAmount = 0 -- ignored for MerchantToken minting
                     }
                (_, otVal) = Value.split(threadTokenValue threadTokenCurSymbol (tpOwnerTokenName tp))
                mintParams = MerchantTokenMintPolicyParams 
                    {
                        mtMerchantTokenName = tn -- the name of the littercoin
                    ,   mtAdminPkh = tpAdminPkh tp  -- the admin pkh who can only mint merch merchantTokens
                    ,   mtOwnerTokenValue = otVal
                    }

            let val     = Value.singleton (merchantTokenCurSymbol mintParams) tn 1
                lookups = Constraints.mintingPolicy (merchantTokenPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val Haskell.<> 
                          Constraints.mustSpendPubKeyOutput oref Haskell.<>
                          Constraints.mustBeSignedBy ownPkh Haskell.<>
                          Constraints.mustPayToPubKey ownPkh (minAda Haskell.<> otVal)
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "mintMerchantToken: Forged %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "mintMerchantToken: Token params %s" (Haskell.show mintParams)



-- | burnMerchantToken burns the merchant approved MerchantToken.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator.  
burnMerchantToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
burnMerchantToken tp = do
    
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "burnToken: No utxo found"
        _ : _ -> do
            let tn = Value.TokenName $ tpMerchantTokenName tp
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     ,  mpTotalAdaAmount = 0 -- ignored for MerchantToken minting
                     ,  mpWithdrawAmount = 0 -- ignored for MerchantToken minting
                     }
                (_, otVal) = Value.split(threadTokenValue threadTokenCurSymbol (tpOwnerTokenName tp))
                mintParams = MerchantTokenMintPolicyParams 
                    {
                        mtMerchantTokenName = tn
                    ,   mtAdminPkh = tpAdminPkh tp
                    ,   mtOwnerTokenValue = otVal
                    }
            let val     = Value.singleton (merchantTokenCurSymbol mintParams) tn (-(tpQty tp))
                lookups = Constraints.mintingPolicy (merchantTokenPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val 
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "burnToken: Burned %s" (Haskell.show val)
            Contract.logInfo @Haskell.String $ printf "burnToken: Burning params %s" (Haskell.show mintParams)


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
addAdaToContract :: TokenParams -> Contract.Contract w s T.Text ()
addAdaToContract tp = do

    ownPkh <- Request.ownPaymentPubKeyHash
    let tn = Value.TokenName $ tpLCTokenName tp
        tn' = Value.TokenName $ tpMerchantTokenName tp
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol (tpThreadTokenName tp))
        (_, otVal) = Value.split(threadTokenValue threadTokenCurSymbol (tpOwnerTokenName tp))
        merchantTokenMintParams = MerchantTokenMintPolicyParams
            {
                mtMerchantTokenName = tn' -- the name of the merchantToken Merchant token
            ,   mtAdminPkh = tpAdminPkh tp
            ,   mtOwnerTokenValue = otVal
            }
        (_, merchTokenVal) = Value.split(merchantTokenValue (merchantTokenCurSymbol merchantTokenMintParams) tn')
        lcParams = LCValidatorParams
            {   lcvAdminPkh = tpAdminPkh tp
            ,   lcvMerchantTokenValue = merchTokenVal
            ,   lcvLCTokenName = tn
            ,   lcvThreadTokenValue = ttVal
            ,   lcvOwnerTokenValue = otVal
            }
        
    (oref, o, lcd@LCDatum{}) <- findLCValidator lcParams threadTokenCurSymbol (tpThreadTokenName tp)
    Contract.logInfo $ "addAdaContract: found littercoin utxo with datum= " ++ Haskell.show lcd
    Contract.logInfo $ "addAdaContract: found littercoin utxo oref= " ++ Haskell.show oref
    Contract.logInfo $ "addAdaContract: littercoin hash= " ++ Haskell.show (lcHash $ PlutusTx.toBuiltinData lcParams)

    let addAda = (lcAdaAmount lcd) + (tpQty tp) 
        lcDatum = LCDatum
            {   lcAdaAmount = addAda                                                 
            ,   lcAmount = lcAmount lcd
            }
        red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ AddAda (tpQty tp)
        dat = PlutusTx.toBuiltinData lcDatum

        lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams) Haskell.<> 
                  Constraints.otherScript (lcValidator $ PlutusTx.toBuiltinData lcParams) Haskell.<> 
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx =      Constraints.mustPayToTheScript dat ((Ada.lovelaceValueOf addAda) Haskell.<> ttVal) Haskell.<> 
                  Constraints.mustSpendScriptOutput oref red Haskell.<>
                  Constraints.mustBeSignedBy ownPkh

    utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    Request.submitTxConfirmed adjustedUtx
    Contract.logInfo $ "addAdaContract: tx submitted successfully= " ++ Haskell.show adjustedUtx


-- | InitSchema type is defined and used by the PAB Contracts
type InitSchema =
        Contract.Endpoint "init" TokenParams


-- | TokenSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mintLC" TokenParams
                   Contract..\/ Contract.Endpoint "addAdaContract" TokenParams
                   Contract..\/ Contract.Endpoint "burnLC" TokenParams
                   Contract..\/ Contract.Endpoint "mintMerchToken" TokenParams
                   Contract..\/ Contract.Endpoint "burnMerchToken" TokenParams
    

-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
initEndpoint :: Contract.Contract (Last (Value.TokenName,Value.TokenName)) InitSchema T.Text ()
initEndpoint = forever
              $ Contract.handleError Contract.logError
              $ Contract.awaitPromise
              $ Contract.endpoint @"init" $ \tp -> initLCValidator tp



-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
useEndpoint :: Contract.Contract () TokenSchema Text ()
useEndpoint = forever $ Contract.handleError Contract.logError $ Contract.awaitPromise $ 
                addAdaContract `Contract.select`
                mintLC `Contract.select` 
                burnLC `Contract.select` 
                mintMerchToken `Contract.select` 
                burnMerchToken 
               
    where
        addAdaContract = Contract.endpoint @"addAdaContract" $ \tp -> addAdaToContract tp 
        mintLC = Contract.endpoint @"mintLC" $ \tp -> mintLCToken tp
        burnLC = Contract.endpoint @"burnLC" $ \tp -> burnLCToken tp 
        mintMerchToken = Contract.endpoint @"mintMerchToken" $ \tp -> mintMerchantToken tp
        burnMerchToken = Contract.endpoint @"burnMerchToken" $ \tp -> burnMerchantToken tp