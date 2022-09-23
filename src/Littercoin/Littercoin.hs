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

module Littercoin.Littercoin
( 
)
where


import           Control.Lens                         (review)
import           Control.Monad                        (forever)
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Text                            (Text)
import qualified Data.Map                             as Map
import           Data.Monoid                          (Last (..))
import qualified Data.OpenApi                         as OpenApi
import           Data.Void                            (Void)
import           GHC.Generics                         (Generic)
import           Ledger                               (getCardanoTxId)
import qualified Ledger.Ada                           as Ada
import qualified Ledger.Address                       as Address
import qualified Ledger.Constraints                   as Constraints
import           Ledger.Params                        (Params)
import qualified Ledger.Tx                            as Tx
import qualified Ledger.Value                         as Value
import           Playground.Schema                    (endpointsToSchemas)
import qualified Plutus.Contract                      as Contract
import qualified Plutus.Contract.Request              as Request
import qualified Plutus.Contract.Wallet               as Wallet
--import qualified Plutus.PAB.Effects.Contract.Builtin  as Builtin
import qualified Plutus.Script.Utils.Typed            as Typed
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as ValidatorsV2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as ContextsV2
import qualified Plutus.V2.Ledger.Tx                  as TxV2
import qualified PlutusTx
import           PlutusPrelude                        (Pretty, void)                             
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       Show (..), String, print, (.),
                                                       Either(..), return)
import           Littercoin.Types                     (MintPolicyRedeemer(..), 
                                                       LCMintPolicyParams(..),
                                                       LCRedeemer(..),
                                                       LCValidatorParams(..),
                                                       NFTMintPolicyParams(..),
                                                       TokenParams(..),
                                                       ThreadTokenRedeemer(..))


-------------------------------------------------
-- ON CHAIN CODE --
-------------------------------------------------

{-# INLINABLE minAda #-}
minAda :: Value.Value
minAda = Ada.lovelaceValueOf 2000000

-- | Create a BuitinByteString from an Integer
{-# INLINEABLE intToBBS #-}
intToBBS :: Integer -> BuiltinByteString
intToBBS y = consByteString (y + 48) emptyByteString -- 48 is ASCII code for '0'


-- | LCDatum is used to record the amount of Littercoin minted and the amount
--   of Ada locked at the smart contract.  This is then used during Littercoin
--   burning to payout the corresponding amount of Ada per Littercoin to the merchant.
data LCDatum = LCDatum
    {   adaAmount           :: !Integer                                         
    ,   lcAmount            :: !Integer                                                                          -- 8
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''LCDatum [('LCDatum, 0)]
PlutusTx.makeLift ''LCDatum


-- | Check that the NFT value is in the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Value.Value -> [ContextsV2.TxOut] -> Bool
validOutputs _ [] = False
validOutputs txVal (x:xs)
    | ContextsV2.txOutValue x == txVal = True
    | otherwise = validOutputs txVal xs
    

{-# INLINABLE mkLittercoinPolicy #-}
mkLittercoinPolicy :: LCMintPolicyParams -> MintPolicyRedeemer -> PlutusV2.ScriptContext -> Bool
mkLittercoinPolicy params (MintPolicyRedeemer polarity withdrawAmount) ctx = 

    case polarity of
        True ->   traceIfFalse "LP1" signedByAdmin 
               && traceIfFalse "LP2" checkMintedAmount 
                
        False ->  traceIfFalse "LP3" checkBurnedAmount 
               && traceIfFalse "LP4" checkNFTValue -- check for merchant NFT

  where

    tn :: Value.TokenName
    tn = lcTokenName params

    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx  

    -- For now this is a single signature witnes, with future plans to make this multi-sig
    signedByAdmin :: Bool
    signedByAdmin =  ContextsV2.txSignedBy info $ Address.unPaymentPubKeyHash (lcAdminPkh params)


    -- Check that the token name minted is greater than 1
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt >= 1
        _               -> False

    -- Check that the token name burned is less than -1
    checkBurnedAmount :: Bool
    checkBurnedAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt <= (-1)
        _               -> False
        
  
    -- Check for NFT Merchant token if burning littercoin
    -- TODO, need to determine Ada at address or split out NFT validation
    checkNFTValue :: Bool
    checkNFTValue = validOutputs (withdrawAda <> (lcNFTTokenValue params)) (PlutusV2.txInfoOutputs info)

        where
            withdrawAda :: Value.Value
            withdrawAda = Ada.lovelaceValueOf (withdrawAmount)
         
   

-- | Wrap the minting policy using the boilerplate template haskell code
lcPolicy :: LCMintPolicyParams -> PlutusV2.MintingPolicy
lcPolicy mp  = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp

  where
    wrap mp' = PSU.V2.mkUntypedMintingPolicy $ mkLittercoinPolicy mp'     


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
lcCurSymbol :: LCMintPolicyParams -> PlutusV2.CurrencySymbol
lcCurSymbol mpParams = PSU.V2.scriptCurrencySymbol $ lcPolicy mpParams 


-- | mkPFTPolicy is the minting policy for creating the approved merchant NFT.
--   When a merchant has one of a merchant approved NFT, they are authorized to spend/burn littercoin.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: NFTMintPolicyParams -> MintPolicyRedeemer -> ContextsV2.ScriptContext -> Bool
mkNFTPolicy params (MintPolicyRedeemer polarity _) ctx = 

    case polarity of
        True ->    traceIfFalse "NFTP1" checkMintedAmount 
                && traceIfFalse "NFTP2" signedByAdmin 
                
        False ->   traceIfFalse "NFTP3" checkBurnedAmount 

  where
    tn :: Value.TokenName
    tn = nftTokenName params

    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx  

    -- For now this is a single signature witnes, with future plans to make this multi-sig
    signedByAdmin :: Bool
    signedByAdmin =  ContextsV2.txSignedBy info $ Address.unPaymentPubKeyHash (nftAdminPkh params)

    -- Check that there is only 1 token minted
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

    -- Check that there is only 1 token burned
    checkBurnedAmount :: Bool
    checkBurnedAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == (-1)
        _               -> False
           

-- | Wrap the minting policy using the boilerplate template haskell code
nftPolicy :: NFTMintPolicyParams -> PlutusV2.MintingPolicy
nftPolicy mp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mp
  where
    wrap mp' = PSU.V2.mkUntypedMintingPolicy $ mkNFTPolicy mp' 


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
{-# INLINABLE nftCurSymbol #-}
nftCurSymbol :: NFTMintPolicyParams -> Value.CurrencySymbol
nftCurSymbol mpParams = PSU.V2.scriptCurrencySymbol $ nftPolicy mpParams 


{-# INLINABLE nftTokenValue #-}
nftTokenValue :: Value.CurrencySymbol -> Value.TokenName -> Value.Value
nftTokenValue cs' tn' = Value.singleton cs' tn' 1



-- | Mint a unique NFT representing a littercoin validator thread token
mkThreadTokenPolicy :: ThreadTokenRedeemer -> ContextsV2.ScriptContext -> Bool
mkThreadTokenPolicy (ThreadTokenRedeemer (TxV2.TxOutRef refHash refIdx)) ctx = 
    traceIfFalse "TP1" txOutputSpent            -- UTxO not consumed
    && traceIfFalse "TP2" checkMintedAmount     -- wrong amount minted    
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    -- True if the pending transaction spends the output
    -- identified by @(refHash, refIdx)@
    -- TODO -- check that refIdx < 256 and fail if not to mitigate 
    -- wrapping back to 0 due to word8 conversion
    txOutputSpent = ContextsV2.spendsOutput info refHash refIdx
    ownSymbol = ContextsV2.ownCurrencySymbol ctx
    minted = ContextsV2.txInfoMint info
    threadToken = sha2_256 $ Tx.getTxId refHash <> intToBBS refIdx

    checkMintedAmount :: Bool
    checkMintedAmount = minted == threadTokenValue ownSymbol (Value.TokenName threadToken) 


{-# INLINABLE wrapThreadTokenPolicy #-}
wrapThreadTokenPolicy :: BuiltinData -> BuiltinData -> ()
wrapThreadTokenPolicy redeemer ctx =
   check $ mkThreadTokenPolicy (PlutusTx.unsafeFromBuiltinData redeemer) (PlutusTx.unsafeFromBuiltinData ctx)


threadTokenPolicy :: PlutusV2.MintingPolicy
threadTokenPolicy = PlutusV2.mkMintingPolicyScript $
     $$(PlutusTx.compile [|| wrapThreadTokenPolicy ||])


{-# INLINABLE threadTokenCurSymbol #-}
threadTokenCurSymbol :: Value.CurrencySymbol
threadTokenCurSymbol = PSU.V2.scriptCurrencySymbol threadTokenPolicy


{-# INLINABLE threadTokenValue #-}
threadTokenValue :: Value.CurrencySymbol -> Value.TokenName -> Value.Value
threadTokenValue cs' tn' = Value.singleton cs' tn' 1

{-
{-# INLINABLE findDatum #-}
-- | Find the data corresponding to a data hash, if there is one
findDatum :: PlutusV2.DatumHash -> Contexts.TxInfo -> Maybe PlutusV2.Datum
findDatum dHash Contexts.TxInfo{Contexts.txInfoData} = snd <$> find f Contexts.txInfoData
  where
    f (dHash', _) = dHash' == dHash
-- | The LC validator is used only for minting, burning, adding and removing of Ada 
--   from the Littercoin smart contract.  
-}

{-# INLINABLE mkLCValidator #-}
mkLCValidator :: LCValidatorParams -> LCDatum -> LCRedeemer -> ContextsV2.ScriptContext -> Bool
mkLCValidator params dat red ctx = 
    case red of
        MintLC qty -> (traceIfFalse "LCV1" $ checkAmountMint qty)        
                  &&  (traceIfFalse "LCV2" $ checkLCDatumMint qty)
                  &&   traceIfFalse "LCV3" signedByAdmin  
        BurnLC qty -> (traceIfFalse "LCV4" $ checkAmountBurn qty)           
                  &&  (traceIfFalse "LCV5" $ checkLCDatumBurn qty)
                  &&   traceIfFalse "LCV7" checkValueAmountBurn                 
                    
        AddAda qty -> (traceIfFalse "LCV8" $ checkLCDatumAdd qty)  
                 &&    traceIfFalse "LCV9" checkValueAmountAdd 
      where        
        tn :: Value.TokenName
        tn = lcvLCTokenName params
        
        info :: ContextsV2.TxInfo
        info = ContextsV2.scriptContextTxInfo ctx  
        -- find the output datum
        outputDat :: LCDatum
        (_, outputDat) = case ContextsV2.getContinuingOutputs ctx of
            [o] -> case TxV2.txOutDatum o of
                TxV2.NoOutputDatum -> traceError "LCV10"       -- no datum present
                TxV2.OutputDatumHash _ -> traceError "LCV11"     -- expecting inline datum and not hash
                TxV2.OutputDatum d -> case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d of
                    Just d' -> (o, d')
                    Nothing  -> traceError "LCV12"       -- error decoding datum data
                    
            _   -> traceError "LCV13"                        -- expected exactly one continuing output
            
{-         
        outputDat :: LCDatum
        (_, outputDat) = case Contexts.getContinuingOutputs ctx of
            [o] -> case Tx.txOutDatumHash o of
                Nothing -> traceError "LCV10"                -- wrong output type
                Just h -> case findDatum h info of
                    Nothing -> traceError "LCV11"           -- datum not found
                    Just (Scripts.Datum d) ->  case PlutusTx.fromBuiltinData d of
                        Just ld' -> (o, ld')
                        Nothing  -> traceError "LCV12"       -- error decoding datum data
            _   -> traceError "LCV13"                        -- expected exactly one continuing output
            
-}     
                            
        -- | Check that the Littercoin token name minted is equal to the amount in the redeemer
        checkAmountMint :: Integer -> Bool
        checkAmountMint q = case Value.flattenValue (ContextsV2.txInfoMint info) of
            [(_, tn', amt)] -> tn' == tn && amt == q
            _               -> False
            
        -- | Check that the difference between LCAmount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumMint :: Integer -> Bool
        checkLCDatumMint q = ((lcAmount outputDat) - (lcAmount dat)) == q
        
        -- | Check that the tx is signed by the admin 
        signedByAdmin :: Bool
        signedByAdmin =  ContextsV2.txSignedBy info $ Address.unPaymentPubKeyHash (lcvAdminPkh params)
          
        -- | Check that the littercoin token name burned is equal to the amount in the redeemer
        checkAmountBurn :: Integer -> Bool
        checkAmountBurn q = case Value.flattenValue (ContextsV2.txInfoMint info) of
            [(_, tn', amt)] -> tn' == tn && amt == (negate q)
            _               -> False
            
        -- | Check that the difference between LCAmount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumBurn :: Integer -> Bool
        checkLCDatumBurn q = ((lcAmount dat) - (lcAmount outputDat)) == q &&
                             ((adaAmount dat) - (adaAmount outputDat) == q * r)
            where
                r :: Integer
                r = divide (adaAmount dat) (lcAmount dat) -- TODO handle 0 lc amount condition
                
        -- | Check that the Ada spent matches Littercoin burned and that the
        -- | merch NFT token is also present
        checkValueAmountBurn :: Bool
        checkValueAmountBurn = validOutputs (newAdaBalance <> (lcvThreadTokenValue params)) (ContextsV2.txInfoOutputs info)
            where     
                newAdaBalance = Ada.lovelaceValueOf (adaAmount outputDat)
                
        -- | Check that the difference between Ada amount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumAdd :: Integer -> Bool
        checkLCDatumAdd q = (adaAmount outputDat) - (adaAmount dat) == q
        
        -- | Check that the Ada added matches increase in the datum
        checkValueAmountAdd :: Bool
        checkValueAmountAdd = validOutputs (addAda <> tt) (ContextsV2.txInfoOutputs info)
            where
                addAda :: Value.Value
                addAda = Ada.lovelaceValueOf (adaAmount outputDat)
                tt :: Value.Value 
                tt = lcvThreadTokenValue params
                
                
-- | Creating a wrapper around littercoin validator for 
--   performance improvements by not using a typed validator
{-# INLINABLE wrapLCValidator #-}
wrapLCValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapLCValidator params dat red ctx =
   check $ mkLCValidator (PlutusV2.unsafeFromBuiltinData params) (PlutusV2.unsafeFromBuiltinData dat) (PlutusV2.unsafeFromBuiltinData red) (PlutusV2.unsafeFromBuiltinData ctx)

untypedLCValidator :: BuiltinData -> PSU.V2.Validator
untypedLCValidator params = PlutusV2.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapLCValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
    
    
-- | We need a typedValidator for offchain mkTxConstraints, so 
-- created it using the untyped validator
typedLCValidator :: BuiltinData -> PSU.V2.TypedValidator Typed.Any
typedLCValidator params =
  ValidatorsV2.unsafeMkTypedValidator $ untypedLCValidator params

mkLCScript :: BuiltinData -> PlutusV2.Script
mkLCScript params = PlutusV2.unValidatorScript $ untypedLCValidator params

lcValidator :: BuiltinData -> PSU.V2.Validator
lcValidator params = Typed.validatorScript $ typedLCValidator params

lcHash :: BuiltinData -> PSU.V2.ValidatorHash
lcHash params = ValidatorsV2.validatorHash $ typedLCValidator params

--untypedLCHash :: BuiltinData -> PSU.V2.ValidatorHash
--untypedLCHash params = ValidatorsV2.validatorHash $ untypedLCValidator params




-------------------------------------------------
-- OFF CHAIN CODE --
-------------------------------------------------

-- | Find the littercoin validator onchain using the lc params and threadtoken
findLCValidator :: LCValidatorParams -> Value.CurrencySymbol -> Value.TokenName -> Contract.Contract w s Text (TxV2.TxOutRef, Tx.ChainIndexTxOut, LCDatum)
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
mintLCToken :: Value.TokenName -> TokenParams -> Contract.Contract () TokenSchema Text ()
mintLCToken tt tp = do

    let tn = Value.TokenName $ tpLCTokenName tp
        tn' = Value.TokenName $ tpNFTTokenName tp
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
burnLCToken :: Value.TokenName -> TokenParams -> Contract.Contract () TokenSchema Text ()
burnLCToken tt tp = do
    
    let tn = Value.TokenName $ tpLCTokenName tp
        tn' = Value.TokenName $ tpNFTTokenName tp
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
            let tn = Value.TokenName $ tpNFTTokenName tp
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
            let tn = Value.TokenName $ tpNFTTokenName tp
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
initLCValidator :: TokenParams -> Contract.Contract (Last Value.TokenName) s Text ()
initLCValidator tp = do
    txOutRef <- Wallet.getUnspentOutput
    let txBS = TxV2.getTxId(TxV2.txOutRefId txOutRef) <> intToBBS(TxV2.txOutRefIdx txOutRef) 
        threadTokenName  = Value.TokenName $ sha2_256 txBS
        (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol threadTokenName)

    Contract.logInfo $ "initLCValidator: thread token name= " ++ show threadTokenName

    ownPkh <- Request.ownFirstPaymentPubKeyHash
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
             Constraints.mustSpendPubKeyOutput txOutRef

    utx <- (Contract.mkTxConstraints lookups tx) >>= Contract.adjustUnbalancedTx
    txId <- getCardanoTxId <$> Contract.submitUnbalancedTx utx   
    Contract.logInfo $ "initLCValidator: tx submitted successfully= " ++ show txId

    Contract.tell $ Last $ Just threadTokenName

-- | Add Ada to the littercoin smart contract  This offchain function is only used by the PAB
--   simulator to test the validation rules of littercoin validator. 
--addAdaContract :: Value.TokenName -> TokenParams -> Contract.Contract () TokenSchema Text ()
addAdaToContract :: Value.TokenName -> TokenParams -> Contract.Contract w s Text ()
addAdaToContract tt tp = do

    let tn = Value.TokenName $ tpLCTokenName tp
        tn' = Value.TokenName $ tpNFTTokenName tp
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
type TokenSchema = Contract.Endpoint "mintLC" (Value.TokenName, TokenParams)
                   Contract..\/ Contract.Endpoint "addAdaContract" (Value.TokenName, TokenParams)
                   Contract..\/ Contract.Endpoint "burnLC" (Value.TokenName, TokenParams)
                   Contract..\/ Contract.Endpoint "mintNFT" TokenParams
                   Contract..\/ Contract.Endpoint "burnNFT" TokenParams
    

-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
initEndpoint :: Contract.Contract (Last Value.TokenName) InitSchema Text ()
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


-- | Setup contracts that are used by the PAB
data Contracts =  InitContract
                | UseContract
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass OpenApi.ToSchema
                      deriving anyclass (FromJSON, ToJSON)

instance Pretty Contracts where
    pretty = viaShow
 
-- | Map PAB Contracts to endpoints
instance Builtin.HasDefinitions Contracts where
    getDefinitions = [ InitContract, UseContract ]
    getSchema =  \case
        InitContract    -> Builtin.endpointsToSchemas @InitSchema 
        UseContract     -> Builtin.endpointsToSchemas @TokenSchema   
   
    getContract = \case
        InitContract    -> Builtin.SomeBuiltin initEndpoint
        UseContract     -> Builtin.SomeBuiltin useEndpoint