{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-} 
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Littercoin.OnChain 
    ( intToBBS
    , lcCurSymbol
    , lcHash
    , lcPolicy
    , lcValidator
    , LCDatum(..)
    , minAda
    , merchantTokenCurSymbol
    , merchantTokenPolicy
    , merchantTokenValue
    , threadTokenCurSymbol
    , threadTokenPolicy 
    , threadTokenValue
    , typedLCValidator
    )
where
    
import           Data.Aeson                             (FromJSON, ToJSON)
import           GHC.Generics                           (Generic)
import qualified Ledger.Ada                             as Ada (lovelaceValueOf)
import qualified Ledger.Address                         as Address (PaymentPubKeyHash(..))
import qualified Ledger.Value                           as Value (CurrencySymbol, flattenValue, singleton, 
                                                        Value)
import           Littercoin.Types                       (MintPolicyRedeemer(..), 
                                                        LCMintPolicyParams(..),
                                                        LCRedeemer(..),
                                                        LCValidatorParams(..),
                                                        MerchantTokenMintPolicyParams(..),
                                                        ThreadTokenRedeemer(..))
import qualified Plutus.Script.Utils.Typed              as Typed (Any, validatorScript)
import qualified Plutus.Script.Utils.V2.Scripts         as PSU.V2  (scriptCurrencySymbol, Validator, 
                                                        ValidatorHash) 
import qualified Plutus.Script.Utils.V2.Typed.Scripts   as PSU.V2 (mkUntypedMintingPolicy, TypedValidator)
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as ValidatorsV2 (unsafeMkTypedValidator, 
                                                        validatorHash)
import qualified Plutus.V2.Ledger.Api                   as PlutusV2 (CurrencySymbol, getDatum,  
                                                        MintingPolicy, mkMintingPolicyScript, mkValidatorScript, 
                                                        scriptContextTxInfo, TokenName(..), TxInfo, 
                                                        txInfoMint, txInfoOutputs, unsafeFromBuiltinData)
import qualified Plutus.V2.Ledger.Contexts              as ContextsV2 (getContinuingOutputs, ownCurrencySymbol, 
                                                        ScriptContext, spendsOutput, TxInfo, txInfoMint, 
                                                        txInfoOutputs, TxOut, txOutValue, txSignedBy)
import qualified Plutus.V2.Ledger.Tx                    as TxV2 (getTxId, OutputDatum(..), TxOut(..), 
                                                        TxOutRef(..))
import qualified PlutusTx                               (applyCode, compile, fromBuiltinData, liftCode, 
                                                        makeIsDataIndexed, makeLift)                       
import           PlutusTx.Prelude                       
import           Prelude                                (Show (..))



-------------------------------------------------
-- ON CHAIN CODE --
-------------------------------------------------

-- | Min Ada is the minimum amout of ada that needs to be included when 
--   transfering/minting a native asset.
{-# INLINABLE minAda #-}
minAda :: Value.Value
minAda = Ada.lovelaceValueOf 2000000

-- | Create a BuitinByteString from an Integer
{-# INLINEABLE intToBBS #-}
intToBBS :: Integer -> BuiltinByteString
intToBBS y = consByteString (y + 48::Integer) emptyByteString -- 48 is ASCII code for '0'


-- | ActionDatum is used to determine what action to perform on the smart contract
--   0 = Add Ada
--   1 = Mint Ada
--   2 = Burn Ada
--   3 = Mint Merchant Token
data ActionDatum = ActionDatum
    {   adAction            :: !Integer
    ,   adAdaAmount           :: !Integer
    ,   adDestination       :: !Address.PaymentPubKeyHash
    ,   adReturn            :: !Address.PaymentPubKeyHash                                                                       -- 8
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''ActionDatum [('ActionDatum, 0)]
PlutusTx.makeLift ''ActionDatum


-- | LCDatum is used to record the amount of Littercoin minted and the amount
--   of Ada locked at the smart contract.  This is then used during Littercoin
--   burning to payout the corresponding amount of Ada per Littercoin to the merchant.
data LCDatum = LCDatum
    {   lcAdaAmount           :: Integer                                         
    ,   lcAmount            :: Integer                                                                         
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''LCDatum [('LCDatum, 0)]
PlutusTx.makeLift ''LCDatum


-- | Check that the specified value is in the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Value.Value -> [ContextsV2.TxOut] -> Bool
validOutputs _ [] = False
validOutputs txVal (x:xs)
    | ContextsV2.txOutValue x == txVal = True
    | otherwise = validOutputs txVal xs
    

-- | The Littercoin minting policy is used to mint and burn littercoins according to the
--   following conditions set out in the policy.     
{-# INLINABLE mkLittercoinPolicy #-}
mkLittercoinPolicy :: LCMintPolicyParams -> MintPolicyRedeemer -> ContextsV2.ScriptContext -> Bool
mkLittercoinPolicy params (MintPolicyRedeemer polarity totalAdaAmount withdrawAmount) ctx = 

    case polarity of
        True ->    traceIfFalse "LP1" signedByAdmin 
                && traceIfFalse "LP2" checkMintedAmount 
                && traceIfFalse "LP3" checkOwnerToken
                && traceIfFalse "LP4" checkThreadToken
                               
        False ->   traceIfFalse "LP5" signedByAdmin
                && traceIfFalse "LP6" checkBurnedAmount
                && traceIfFalse "LP7" checkMerchantToken 
                && traceIfFalse "LP8" checkThreadToken
  where

    tn :: PlutusV2.TokenName
    tn = lcTokenName params

    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx  

    signedByAdmin :: Bool
    signedByAdmin =  ContextsV2.txSignedBy info $ Address.unPaymentPubKeyHash (lcAdminPkh params)

    -- | Check that the token name minted is greater than 1
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt >= 1
        _               -> False


    -- | Check that the token name burned is less than -1
    checkBurnedAmount :: Bool
    checkBurnedAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt <= (-1)
        _               -> False
        
    -- Check for Owner token for minting of littercoin
    checkOwnerToken :: Bool
    checkOwnerToken = validOutputs (minAda <> (lcOwnerTokenValue params)) (PlutusV2.txInfoOutputs info)
    

    -- | Check for MerchantToken Merchant token if burning littercoin
    checkMerchantToken :: Bool
    checkMerchantToken = validOutputs (withdrawAda <> (lcMerchantTokenValue params)) (PlutusV2.txInfoOutputs info)

        where
            withdrawAda :: Value.Value
            withdrawAda = Ada.lovelaceValueOf (withdrawAmount)


    -- | Check that thread token is part of the transaction output.   If so, then this confirms that
    --   the littercoin validator has also been called and spent as part of this minting transaction.
    checkThreadToken :: Bool
    checkThreadToken = validOutputs (totalAda <> (lcThreadTokenValue params)) (PlutusV2.txInfoOutputs info)

        where
            totalAda :: Value.Value
            totalAda = Ada.lovelaceValueOf (totalAdaAmount)


-- | Wrap the minting policy using the boilerplate template haskell code
lcPolicy :: LCMintPolicyParams -> PlutusV2.MintingPolicy
lcPolicy mp  = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp

  where
    wrap mp' = PSU.V2.mkUntypedMintingPolicy $ mkLittercoinPolicy mp'     


-- | Provide the currency symbol of the minting policy which requires LCMintPolicyParams
--   as a parameter to the minting policy.
lcCurSymbol :: LCMintPolicyParams -> PlutusV2.CurrencySymbol
lcCurSymbol mpParams = PSU.V2.scriptCurrencySymbol $ lcPolicy mpParams 


-- | mkPFTPolicy is the minting policy for creating the approved merchant MerchantToken.
--   When a merchant has one of a merchant approved MerchantToken, they are authorized to burn littercoin
--   and receive Ada from the littercoin smart contract.
{-# INLINABLE mkMerchantTokenPolicy #-}
mkMerchantTokenPolicy :: MerchantTokenMintPolicyParams -> MintPolicyRedeemer -> ContextsV2.ScriptContext -> Bool
mkMerchantTokenPolicy params (MintPolicyRedeemer polarity _ _) ctx = 

    case polarity of
        True ->    traceIfFalse "MTP1" checkMintedAmount 
                -- && traceIfFalse "MTP2" signedByAdmin 
                && traceIfFalse "MTP3" checkOwnerToken 
                
        False ->   traceIfFalse "MTP4" checkBurnedAmount 

  where
    tn :: PlutusV2.TokenName
    tn = mtTokenName params

    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx  

    -- | For now this is a single signature witnes, with future plans to make this multi-sig
    --signedByAdmin :: Bool
    --signedByAdmin =  ContextsV2.txSignedBy info $ Address.unPaymentPubKeyHash (mtAdminPkh params)

    -- | Check that there is only 1 token minted
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

    -- | Check that there is only 1 token burned
    checkBurnedAmount :: Bool
    checkBurnedAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == (-1)
        _               -> False

    -- Check for Owner token for minting of merchant token
    checkOwnerToken :: Bool
    checkOwnerToken = validOutputs (minAda <> (mtOwnerTokenValue params)) (PlutusV2.txInfoOutputs info)

           

-- | Wrap the minting policy using the boilerplate template haskell code
merchantTokenPolicy :: MerchantTokenMintPolicyParams -> PlutusV2.MintingPolicy
merchantTokenPolicy mp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mp
  where
    wrap mp' = PSU.V2.mkUntypedMintingPolicy $ mkMerchantTokenPolicy mp' 


-- | Provide the currency symbol of the minting policy which requires MerchantTokenMintPolicyParams
--   as a parameter to the minting policy
{-# INLINABLE merchantTokenCurSymbol #-}
merchantTokenCurSymbol :: MerchantTokenMintPolicyParams -> Value.CurrencySymbol
merchantTokenCurSymbol mpParams = PSU.V2.scriptCurrencySymbol $ merchantTokenPolicy mpParams 


-- | Provide the Value of a merchant Token
{-# INLINABLE merchantTokenValue #-}
merchantTokenValue :: Value.CurrencySymbol -> PlutusV2.TokenName -> Value.Value
merchantTokenValue cs' tn' = Value.singleton cs' tn' 1


-- | Mint a unique MerchantToken representing a littercoin validator thread token
mkThreadTokenPolicy :: ThreadTokenRedeemer -> ContextsV2.ScriptContext -> Bool
mkThreadTokenPolicy (ThreadTokenRedeemer (TxV2.TxOutRef refHash refIdx)) ctx = 
        traceIfFalse "TP1" maxRefIdx             -- refIdx must be less than 256
     && traceIfFalse "TP2" txOutputSpent         -- UTxO not consumed
     && traceIfFalse "TP3" checkMintedAmount     -- wrong amount minted  
  
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    maxRefIdx :: Bool
    maxRefIdx = refIdx < 256
    
    txOutputSpent :: Bool
    txOutputSpent = ContextsV2.spendsOutput info refHash refIdx
    
    ownSymbol = ContextsV2.ownCurrencySymbol ctx
    minted = ContextsV2.txInfoMint info
    threadToken = sha2_256 $ TxV2.getTxId refHash <> intToBBS refIdx
    ownerToken = sha2_256 threadToken

    checkMintedAmount :: Bool
    checkMintedAmount = minted == threadTokenValue ownSymbol (PlutusV2.TokenName threadToken) 
                                <> threadTokenValue ownSymbol (PlutusV2.TokenName ownerToken)


-- | Wrap the minting policy using the boilerplate template haskell code
threadTokenPolicy :: PlutusV2.MintingPolicy
threadTokenPolicy = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = PSU.V2.mkUntypedMintingPolicy mkThreadTokenPolicy 


-- | Provide the currency symbol of the thread token minting policy
{-# INLINABLE threadTokenCurSymbol #-}
threadTokenCurSymbol :: Value.CurrencySymbol
threadTokenCurSymbol = PSU.V2.scriptCurrencySymbol threadTokenPolicy


-- | Provide the value of a thread token
{-# INLINABLE threadTokenValue #-}
threadTokenValue :: Value.CurrencySymbol -> PlutusV2.TokenName -> Value.Value
threadTokenValue cs' tn' = Value.singleton cs' tn' 1


-- | The Littercoin validator allows for minting and burning of Littercoin
--  and adding Ada to the smart contract.   Both minting and burning increment or 
--  decrement the amount of Littercoin recorded in the datum respectively.   When
--  burning Littercoin, the amount of Ada is calculated based on the ratio of 
--  Ada : Littercoin, and the correpsponding amount of Ada is sent to the merchant
--  wallet who would call this smart contract.
{-# INLINABLE mkLCValidator #-}
mkLCValidator :: LCValidatorParams -> LCDatum -> LCRedeemer -> ContextsV2.ScriptContext -> Bool
mkLCValidator params dat red ctx = 

    case red of
        MintLC qty -> (traceIfFalse "LCV1" $ checkAmountMint qty)        
                  &&  (traceIfFalse "LCV2" $ checkLCDatumMint qty)
                  &&   traceIfFalse "LCV3" signedByAdmin
                  &&   traceIfFalse "LCV4" checkOwnerToken
                  &&   traceIfFalse "LCV5" checkValueAmountMint  

        BurnLC qty -> (traceIfFalse "LCV6" $ checkAmountBurn qty)           
                  &&  (traceIfFalse "LCV7" $ checkLCDatumBurn qty)
                  &&   traceIfFalse "LCV8" signedByAdmin
                  &&   traceIfFalse "LCV9" checkValueAmountBurn 
                  &&   traceIfFalse "LCV10" checkMerchantToken                
                    
        AddAda qty -> (traceIfFalse "LCV11" $ checkLCDatumAdd qty)
                  &&   traceIfFalse "LCV12" signedByAdmin  
                  &&   traceIfFalse "LCV13" checkValueAmountAdd  
      where        
        tn :: PlutusV2.TokenName
        tn = lcvTokenName params
        
        info :: ContextsV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx  
        
        -- | Find the output datum
        outputDat :: LCDatum
        (_, outputDat) = case ContextsV2.getContinuingOutputs ctx of
            [o] -> case TxV2.txOutDatum o of
                TxV2.NoOutputDatum -> traceError "LCV10"       -- no datum present
                TxV2.OutputDatumHash _ -> traceError "LCV11"     -- expecting inline datum and not hash
                TxV2.OutputDatum d -> case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d of
                    Just d' -> (o, d')
                    Nothing  -> traceError "LCV12"       -- error decoding datum data
                    
            _   -> traceError "LCV13"                        -- expected exactly one continuing output
            
                            
        -- | Check that the Littercoin token name minted is equal to the amount in the redeemer
        checkAmountMint :: Integer -> Bool
        checkAmountMint q = case Value.flattenValue (ContextsV2.txInfoMint info) of
            [(_, tn', amt)] -> tn' == tn && amt == q
            _               -> False
            
        -- | Check that the difference between LCAmount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumMint :: Integer -> Bool
        checkLCDatumMint q = ((lcAmount outputDat) - (lcAmount dat)) == q

        -- | Check that the Ada spent matches and that the thread token is also present
        checkValueAmountMint :: Bool
        checkValueAmountMint = validOutputs (adaBalance <> (lcvThreadTokenValue params)) (ContextsV2.txInfoOutputs info)

            where     
                adaBalance = Ada.lovelaceValueOf (lcAdaAmount outputDat)
        
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
                             ((lcAdaAmount dat) - (lcAdaAmount outputDat) == q * r)
            where
                r :: Integer
                r = divide (lcAdaAmount dat) (lcAmount dat) -- TODO handle 0 lc amount condition
                
        -- | Check that the Ada spent matches Littercoin burned and that the
        -- | merch MerchantToken token is also present
        checkValueAmountBurn :: Bool
        checkValueAmountBurn = validOutputs (newAdaBalance <> (lcvThreadTokenValue params)) (ContextsV2.txInfoOutputs info)
            where     
                newAdaBalance = Ada.lovelaceValueOf (lcAdaAmount outputDat)
                
        -- | Check that the difference between Ada amount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumAdd :: Integer -> Bool
        checkLCDatumAdd q = (lcAdaAmount outputDat) - (lcAdaAmount dat) == q
        
        -- | Check that the Ada added matches increase in the datum
        checkValueAmountAdd :: Bool
        checkValueAmountAdd = validOutputs (addAda <> tt) (ContextsV2.txInfoOutputs info)
            where
                addAda :: Value.Value
                addAda = Ada.lovelaceValueOf (lcAdaAmount outputDat)

                tt :: Value.Value 
                tt = lcvThreadTokenValue params


        -- Check for Owner token required for minting
        checkOwnerToken :: Bool
        checkOwnerToken = validOutputs (minAda <> (lcvOwnerTokenValue params)) (ContextsV2.txInfoOutputs info)


        -- Check for Merchant token if burning littercoin
        checkMerchantToken :: Bool
        checkMerchantToken = validOutputs (withdrawAda <> (lcvMerchantTokenValue params)) (ContextsV2.txInfoOutputs info)

            where
                withdrawAda :: Value.Value
                withdrawAda = Ada.lovelaceValueOf ((lcAdaAmount dat) - (lcAdaAmount outputDat))

                
                
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


lcValidator :: BuiltinData -> PSU.V2.Validator
lcValidator params = Typed.validatorScript $ typedLCValidator params


lcHash :: BuiltinData -> PSU.V2.ValidatorHash
lcHash params = ValidatorsV2.validatorHash $ typedLCValidator params

