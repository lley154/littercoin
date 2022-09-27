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

module Littercoin.OffChain 
    (
      initEndpoint
    , InitSchema
    , TokenSchema
    , useEndpoint
    )

where
    
import           Control.Monad                        (forever)
import           Data.Text                            (Text)
import qualified Data.Map                             as Map
import           Data.Monoid                          (Last (..))
import           Data.Void                            (Void)
import           Ledger                               (getCardanoTxId)
import qualified Ledger.Ada                           as Ada
import qualified Ledger.Address                       as Address
import qualified Ledger.Constraints                   as Constraints
import qualified Ledger.Tx                            as Tx
import qualified Ledger.Value                         as Value
import           Littercoin.OnChain
import           Littercoin.Types
import qualified Plutus.Contract                      as Contract
import qualified Plutus.Contract.Request              as Request
import qualified Plutus.Contract.Wallet               as Wallet
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Tx                  as TxV2
import qualified PlutusTx
import           PlutusPrelude                        (void)                      
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (Semigroup (..),
                                                       Show (..), String)
import           Littercoin.Types                     (MintPolicyRedeemer(..), 
                                                       LCMintPolicyParams(..),
                                                       LCRedeemer(..),
                                                       LCValidatorParams(..),
                                                       NFTMintPolicyParams(..),
                                                       TokenParams(..),
                                                       ThreadTokenRedeemer(..))


-------------------------------------------------
-- OFF CHAIN CODE --
-------------------------------------------------

-- | Find the littercoin validator onchain using the lc params and threadtoken
findLCValidator :: LCValidatorParams -> Value.CurrencySymbol -> PlutusV2.TokenName -> Contract.Contract w s Text (TxV2.TxOutRef, Tx.ChainIndexTxOut, LCDatum)
findLCValidator params cs tn = do
    utxos <- Contract.utxosAt $ Address.scriptHashAddress $ lcHash $ PlutusTx.toBuiltinData params
    
    
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (Tx._ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case Tx._ciTxOutScriptDatum o of
            (_, Nothing)          -> Contract.throwError "findLCValidator: datum missing"
            (_, Just (PlutusV2.Datum d)) -> case PlutusTx.fromBuiltinData d of
                Nothing -> Contract.throwError "findLCValidator: datum has wrong type"
                Just d'@LCDatum{} -> return (oref, o, d')
        _           -> Contract.throwError "findLCValidator: utxo not found"

{-

    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (Tx._ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case Tx._ciTxOutDatum o of
            Left _          -> Contract.throwError "findLCValidator: datum missing"
            Right (Scripts.Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> Contract.throwError "findLCValidator: datum has wrong type"
                Just d@LCDatum{} -> return (oref, o, d)
        _           -> Contract.throwError "findLCValidator: utxo not found"
-}

-- | mintLC mints Littercoin tokens and increments the Littercoin counter
--   in the LC validator datum.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintLCToken :: PlutusV2.TokenName -> TokenParams -> Contract.Contract () TokenSchema Text ()
mintLCToken tt tp = do

    let tn = PlutusV2.TokenName $ tpLCTokenName tp
        tn' = PlutusV2.TokenName $ tpNFTTokenName tp
        nftMintParams = NFTMintPolicyParams
            {
                nftTokenName = tn' -- the name of the NFT Merchant token
            ,   nftAdminPkh = tpAdminPkh tp
            }
            
        (_, nftTokVal) = Value.split(nftTokenValue (nftCurSymbol nftMintParams) tn')
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol tt)
        lcParams = LCValidatorParams
            {   lcvAdminPkh         = tpAdminPkh tp
            ,   lcvNFTTokenValue    = nftTokVal
            ,   lcvLCTokenName      = tn
            ,   lcvThreadTokenValue = ttVal
            }
        
    (orefLC, oLC, lcd@LCDatum{}) <- findLCValidator lcParams threadTokenCurSymbol tt
    Contract.logInfo $ "mintLCToken: found littercoin utxo with datum= " ++ show lcd
    Contract.logInfo $ "mintLCToken: found littercoin utxo oref= " ++ show orefLC
    Contract.logInfo $ "mintLCToken: littercoin hash= " ++ show (lcHash $ PlutusTx.toBuiltinData lcParams)

    let lcDatum = LCDatum
            {   adaAmount = adaAmount lcd                                                  
            ,   lcAmount = (lcAmount lcd) + (tpQty tp)
            }
        redLC = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintLC (tpQty tp)
        datLC = PlutusTx.toBuiltinData lcDatum


    ownPkh <- Request.ownFirstPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @String "mintToken: No utxo found"
        oref : _ -> do
            let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     ,  mpWithdrawAmount = 0 -- ignored during minting   
                     }
                mintParams = LCMintPolicyParams 
                    {
                        lcTokenName = tn -- the name of the littercoin
                    ,   lcAdminPkh = tpAdminPkh tp  -- the admin pkh who can only mint littercoins
                    ,   lcNFTTokenValue = nftTokVal  -- this contains the NFT that merchants used for burning
                    }

            let val     = Value.singleton (lcCurSymbol mintParams) tn (tpQty tp)
                lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams) <> 
                          Constraints.plutusV2OtherScript (lcValidator $ PlutusTx.toBuiltinData lcParams) <> 
                          Constraints.unspentOutputs (Map.singleton orefLC oLC) <> 
                          Constraints.plutusV2MintingPolicy (lcPolicy mintParams) <> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustPayToTheScript datLC (Ada.lovelaceValueOf (adaAmount lcd) <> ttVal) <> 
                          Constraints.mustSpendScriptOutput orefLC redLC <>
                          Constraints.mustMintValueWithRedeemer red val <> 
                          Constraints.mustSpendPubKeyOutput oref <>
                          Constraints.mustBeSignedBy ownPkh
          
            --utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
            --let adjustedUtx = Constraints.adjustUnbalancedTx utx
            --Request.submitTxConfirmed adjustedUtx
            
            utx <- (Contract.mkTxConstraints lookups tx) >>= Contract.adjustUnbalancedTx
            txId <- getCardanoTxId <$> Contract.submitUnbalancedTx utx
            
            Contract.logInfo $ "mintLCToken: tx submitted successfully= " ++ show txId

-- | burnLC burns littercoin tokens.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator.  
burnLCToken :: PlutusV2.TokenName -> TokenParams -> Contract.Contract () TokenSchema Text ()
burnLCToken tt tp = do
    
    let tn = PlutusV2.TokenName $ tpLCTokenName tp
        tn' = PlutusV2.TokenName $ tpNFTTokenName tp
        nftMintParams = NFTMintPolicyParams
            {
                nftTokenName = tn' -- the name of the NFT Merchant token
            ,   nftAdminPkh = tpAdminPkh tp
            }
        (_, nftTokVal) = Value.split(nftTokenValue (nftCurSymbol nftMintParams) tn')
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol tt)
        lcParams = LCValidatorParams
            {   lcvAdminPkh         = tpAdminPkh tp
            ,   lcvNFTTokenValue    = nftTokVal
            ,   lcvLCTokenName      = tn
            ,   lcvThreadTokenValue = ttVal
            }
        
    (orefLC, oLC, lcd@LCDatum{}) <- findLCValidator lcParams threadTokenCurSymbol tt
    Contract.logInfo $ "burnLCToken: found littercoin utxo with datum= " ++ show lcd
    Contract.logInfo $ "burnLCToken: found littercoin utxo oref= " ++ show orefLC
    Contract.logInfo $ "burnLCToken: littercoin hash= " ++ show (lcHash $ PlutusTx.toBuiltinData lcParams)

    let withdrawAda = (divide (adaAmount lcd) (lcAmount lcd)) * (tpQty tp) 
        lcDatum = LCDatum
            {   adaAmount = (adaAmount lcd) - withdrawAda                                                
            ,   lcAmount = (lcAmount lcd) - (tpQty tp)
            }
        redLC = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ BurnLC (tpQty tp)
        datLC = PlutusTx.toBuiltinData lcDatum
    
    ownPkh <- Request.ownFirstPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @String "burnLCToken: No utxo found"
        _ : _ -> do
            let pkh = tpAdminPkh tp
                red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     ,  mpWithdrawAmount = withdrawAda
                     }
                mintParams = LCMintPolicyParams 
                    {
                        lcTokenName = tn
                    ,   lcAdminPkh = pkh
                    ,   lcNFTTokenValue = nftTokVal
                    }
            let val     = Value.singleton (lcCurSymbol mintParams) tn (-(tpQty tp))
                lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams) <> 
                          Constraints.plutusV2OtherScript (lcValidator $ PlutusTx.toBuiltinData lcParams) <> 
                          Constraints.unspentOutputs (Map.singleton orefLC oLC) <> 
                          Constraints.plutusV2MintingPolicy (lcPolicy mintParams) <> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustPayToTheScript datLC (Ada.lovelaceValueOf ((adaAmount lcd) - withdrawAda) <> ttVal) <> 
                          Constraints.mustSpendScriptOutput orefLC redLC <>
                          Constraints.mustMintValueWithRedeemer red val <>
                          Constraints.mustPayToPubKey pkh ((Ada.lovelaceValueOf withdrawAda) <> nftTokVal)

            --utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
            --let adjustedUtx = Constraints.adjustUnbalancedTx utx
            --Request.submitTxConfirmed adjustedUtx

            utx <- (Contract.mkTxConstraints lookups tx) >>= Contract.adjustUnbalancedTx
            txId <- getCardanoTxId <$> Contract.submitUnbalancedTx utx
            
            Contract.logInfo $ "burnLCToken: tx submitted successfully= " ++ show txId


-- | mintNFT mints the merchant approved NFT.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintNFTToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
mintNFTToken tp = do
     
    ownPkh <- Request.ownFirstPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @String "mintToken: No utxo found"
        oref : _ -> do
            let tn = PlutusV2.TokenName $ tpNFTTokenName tp
                red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     ,  mpWithdrawAmount = 0 -- ignored for NFT minting
                     }
                mintParams = NFTMintPolicyParams 
                    {
                        nftTokenName = tn -- the name of the littercoin
                    ,   nftAdminPkh = tpAdminPkh tp  -- the admin pkh who can only mint merch NFTs
                    }

            let val     = Value.singleton (nftCurSymbol mintParams) tn (tpQty tp)
                lookups = Constraints.plutusV2MintingPolicy (nftPolicy mintParams) <> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val <> 
                          Constraints.mustSpendPubKeyOutput oref <>
                          Constraints.mustBeSignedBy ownPkh

            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo $ "mintNFT: Forged: " ++ show val
            Contract.logInfo $ "mintNFT: Token params: " ++ show mintParams



-- | burnNFT burns the merchant approved NFT.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator.  
burnNFTToken :: TokenParams -> Contract.Contract () TokenSchema Text ()
burnNFTToken tp = do
    
    ownPkh <- Request.ownFirstPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @String "burnToken: No utxo found"
        _ : _ -> do
            let tn = PlutusV2.TokenName $ tpNFTTokenName tp
                red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False -- burn token
                     ,  mpWithdrawAmount = 0 -- ignored for NFT burning
                     }
                mintParams = NFTMintPolicyParams 
                    {
                        nftTokenName = tn
                    ,   nftAdminPkh = tpAdminPkh tp
                    }
            let val     = Value.singleton (nftCurSymbol mintParams) tn (-(tpQty tp))
                lookups = Constraints.plutusV2MintingPolicy  (nftPolicy mintParams) <> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red val 

            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo $ "burnNFT: Burned: " ++ show val
            Contract.logInfo $ "burnNFT: Token params: " ++ show mintParams



-- | Initialize the littercoin contract
initLCValidator :: TokenParams -> Contract.Contract (Last PlutusV2.TokenName) s Text ()
initLCValidator tp = do
    txOutRef <- Wallet.getUnspentOutput
    let txBS = TxV2.getTxId(TxV2.txOutRefId txOutRef) <> intToBBS(TxV2.txOutRefIdx txOutRef) 
        threadTokenName  = PlutusV2.TokenName $ sha2_256 txBS
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol threadTokenName)

    Contract.logInfo $ "initLCValidator: thread token name= " ++ show threadTokenName

    ownPkh <- Request.ownFirstPaymentPubKeyHash
    utxo <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)

    let tn = PlutusV2.TokenName $ tpLCTokenName tp
        tn' = PlutusV2.TokenName $ tpNFTTokenName tp
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
            ,   lcvThreadTokenValue = ttVal
            }
        lcDatum = LCDatum 
            {   adaAmount = 0                                         
            ,   lcAmount = 0
            }

        red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ ThreadTokenRedeemer txOutRef
        dat = PlutusTx.toBuiltinData lcDatum
        lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams) <>
                  Constraints.plutusV2MintingPolicy threadTokenPolicy <>
                  Constraints.unspentOutputs utxo
        tx = Constraints.mustPayToTheScript dat (minAda <> ttVal) <>
             Constraints.mustMintValueWithRedeemer red ttVal <>
             Constraints.mustIncludeDatum (PlutusV2.Datum dat) <>
             Constraints.mustSpendPubKeyOutput txOutRef

    utx <- (Contract.mkTxConstraints lookups tx) >>= Contract.adjustUnbalancedTx
    txId <- getCardanoTxId <$> Contract.submitUnbalancedTx utx   
    Contract.logInfo $ "initLCValidator: tx submitted successfully= " ++ show txId

    Contract.tell $ Last $ Just threadTokenName

-- | Add Ada to the littercoin smart contract  This offchain function is only used by the PAB
--   simulator to test the validation rules of littercoin validator. 
--addAdaContract :: PlutusV2.TokenName -> TokenParams -> Contract.Contract () TokenSchema Text ()
addAdaToContract :: PlutusV2.TokenName -> TokenParams -> Contract.Contract w s Text ()
addAdaToContract tt tp = do

    let tn = PlutusV2.TokenName $ tpLCTokenName tp
        tn' = PlutusV2.TokenName $ tpNFTTokenName tp
        nftMintParams = NFTMintPolicyParams
            {
                nftTokenName = tn' -- the name of the NFT Merchant token
            ,   nftAdminPkh = tpAdminPkh tp
            }
        (_, nftTokVal) = Value.split(nftTokenValue (nftCurSymbol nftMintParams) tn')
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol tt)
        lcParams = LCValidatorParams
            {   lcvAdminPkh         = tpAdminPkh tp
            ,   lcvNFTTokenValue    = nftTokVal
            ,   lcvLCTokenName      = tn
            ,   lcvThreadTokenValue = ttVal
            }
        
    (oref, o, lcd@LCDatum{}) <- findLCValidator lcParams threadTokenCurSymbol tt
    Contract.logInfo $ "addAdaContract: found littercoin utxo with datum= " ++ show lcd
    Contract.logInfo $ "addAdaContract: found littercoin utxo oref= " ++ show oref
    Contract.logInfo $ "addAdaContract: littercoin hash= " ++ show (lcHash $ PlutusTx.toBuiltinData lcParams)

    let addAda = (adaAmount lcd) + (tpQty tp) 
        lcDatum = LCDatum
            {   adaAmount = addAda                                                 
            ,   lcAmount = lcAmount lcd
            }
        red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ AddAda (tpQty tp)
        dat = PlutusTx.toBuiltinData lcDatum

        lookups = Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams) <> 
                  Constraints.plutusV2OtherScript (lcValidator $ PlutusTx.toBuiltinData lcParams) <> 
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx =      Constraints.mustPayToTheScript dat ((Ada.lovelaceValueOf addAda) <> ttVal) <> 
                  Constraints.mustSpendScriptOutput oref red

    utx <- (Contract.mkTxConstraints lookups tx) >>= Contract.adjustUnbalancedTx
    txId <- getCardanoTxId <$> Contract.submitUnbalancedTx utx   
    Contract.logInfo $ "addAdaContract: tx submitted successfully= " ++ show txId



-- | InitSchema type is defined and used by the PAB Contracts
type InitSchema =
        Contract.Endpoint "init" TokenParams


-- | TokenSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mintLC" (PlutusV2.TokenName, TokenParams)
                   Contract..\/ Contract.Endpoint "addAdaContract" (PlutusV2.TokenName, TokenParams)
                   Contract..\/ Contract.Endpoint "burnLC" (PlutusV2.TokenName, TokenParams)
                   Contract..\/ Contract.Endpoint "mintNFT" TokenParams
                   Contract..\/ Contract.Endpoint "burnNFT" TokenParams
    

-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
initEndpoint :: Contract.Contract (Last PlutusV2.TokenName) InitSchema Text ()
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
                mintNFT `Contract.select` 
                burnNFT 
               
    where
        addAdaContract = Contract.endpoint @"addAdaContract" $ \(tt, tp) -> addAdaToContract tt tp 
        mintLC = Contract.endpoint @"mintLC" $ \(tt, tp) -> mintLCToken tt tp
        burnLC = Contract.endpoint @"burnLC" $ \(tt, tp) -> burnLCToken tt tp 
        mintNFT = Contract.endpoint @"mintNFT" $ \(tp) -> mintNFTToken tp
        burnNFT = Contract.endpoint @"burnNFT" $ \(tp) -> burnNFTToken tp



