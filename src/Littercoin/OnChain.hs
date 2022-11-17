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
    (
      intToBBS
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
    ) where

import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           Littercoin.Types                   (LCMintPolicyParams(..), LCRedeemer(..), LCValidatorParams(..), MerchantTokenMintPolicyParams(..), MintPolicyRedeemer(..), ThreadTokenRedeemer(..))
import           Ledger                             (mkMintingPolicyScript, ScriptContext(..), scriptCurrencySymbol, 
                                                     TxInfo(..),  txSignedBy, TxId(getTxId ))
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import qualified Ledger.Address as Address          (PaymentPubKeyHash(..))
import qualified Ledger.Contexts as Contexts        (getContinuingOutputs, ownCurrencySymbol, scriptCurrencySymbol, spendsOutput, TxOut)
import qualified Ledger.Scripts as Scripts          (Datum(..), DatumHash, mkMintingPolicyScript, mkValidatorScript, Script, Validator, ValidatorHash, validatorHash)                                                  
import qualified Ledger.Tx as Tx                    (TxOut(..), TxOutRef(..))
import qualified Ledger.Typed.Scripts.Validators as Validators (unsafeMkTypedValidator)
import qualified Ledger.Typed.TypeUtils as TypeUtils (Any)
import qualified Ledger.Typed.Scripts as TScripts   (MintingPolicy, TypedValidator, validatorScript, validatorHash, wrapMintingPolicy)
import qualified Ledger.Value as Value              (CurrencySymbol, flattenValue, singleton, TokenName(..), Value)
import           Plutus.V1.Ledger.Api as Ledger     (unsafeFromBuiltinData, unValidatorScript)
import qualified PlutusTx                           (applyCode, compile, fromBuiltinData, liftCode, makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, BuiltinData, check, consByteString, divide, emptyByteString, find, Integer, Maybe(..), negate, otherwise, snd, sha2_256, traceIfFalse, traceError, (&&), (==), ($), (<=), (>=), (<>), (<$>), (-), (*), (+), (<))
import qualified Prelude as Haskell                 (Show)

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

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
    {   lcAdaAmount             :: !Integer                                         
    ,   lcAmount                :: !Integer                                                                          -- 8
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''LCDatum [('LCDatum, 0)]
PlutusTx.makeLift ''LCDatum


-- | ActionDatum is used to determine what action to perform on the smart contract
--   0 = Add Ada
--   1 = Mint Ada
--   2 = Burn Ada
--   3 = Mint Merchant Token
data ActionDatum = ActionDatum
    {   adAction            :: !Integer
    ,   adaAmount           :: !Integer
    ,   adDestination       :: !Address.PaymentPubKeyHash
    ,   adReturn            :: !Address.PaymentPubKeyHash                                                                       -- 8
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''ActionDatum [('ActionDatum, 0)]
PlutusTx.makeLift ''ActionDatum


-- | Check that the value is in the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Value.Value -> [Contexts.TxOut] -> Bool
validOutputs _ [] = False
validOutputs txVal (x:xs)
    | Tx.txOutValue x == txVal = True
    | otherwise = validOutputs txVal xs
                             

-- | The mkLittercoinPolicy is the minting policy validator for minting and burning
--   of littercoin tokens.  It is a parameterized validator so each policy
--   id is unqiue based on the admin pkh provided.

{-# INLINABLE mkLittercoinPolicy #-}
mkLittercoinPolicy :: LCMintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkLittercoinPolicy params (MintPolicyRedeemer mpPolarity mpTotalAdaAmount mpWithdrawAmount) ctx = 

    case mpPolarity of
        True ->    traceIfFalse "LP1" signedByAdmin 
                && traceIfFalse "LP2" checkMintedAmount 
                && traceIfFalse "LP3" checkOwnerToken
                && traceIfFalse "LP4" checkThreadToken
                               
        False ->   traceIfFalse "LP5" signedByAdmin
                && traceIfFalse "LP6" checkBurnedAmount
                && traceIfFalse "LP7" checkMerchantToken 
                && traceIfFalse "LP8" checkThreadToken
                
  where

    tn :: Value.TokenName
    tn = lcTokenName params

    info :: TxInfo
    info = scriptContextTxInfo ctx  
    
    signedByAdmin :: Bool
    signedByAdmin =  txSignedBy info $ Address.unPaymentPubKeyHash (lcAdminPkh params)

    -- Check that the token name minted is greater than 1
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt >= 1
        _               -> False

    -- Check that the token name burned is less than -1
    checkBurnedAmount :: Bool
    checkBurnedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt <= (-1)
        _               -> False

    -- Check for Owner token for minting of littercoin
    checkOwnerToken :: Bool
    checkOwnerToken = validOutputs (minAda <> (lcOwnerTokenValue params)) (txInfoOutputs info)

    -- Check for Merchant token if burning littercoin
    checkMerchantToken :: Bool
    checkMerchantToken = validOutputs (withdrawAda <> (lcMerchantTokenValue params)) (txInfoOutputs info)

        where
            withdrawAda :: Value.Value
            withdrawAda = Ada.lovelaceValueOf mpWithdrawAmount

    -- Check for thread token to confirm that the littercoin validator is also 
    -- included in the transaction
    checkThreadToken :: Bool
    checkThreadToken = validOutputs (lcAdaAmount <> (lcThreadTokenValue params)) (txInfoOutputs info)

        where
            lcAdaAmount :: Value.Value
            lcAdaAmount = Ada.lovelaceValueOf mpTotalAdaAmount


-- | Wrap the minting policy using the boilerplate template haskell code
lcPolicy :: LCMintPolicyParams -> TScripts.MintingPolicy
lcPolicy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> TScripts.wrapMintingPolicy $ mkLittercoinPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
lcCurSymbol :: LCMintPolicyParams -> Value.CurrencySymbol
lcCurSymbol mpParams = scriptCurrencySymbol $ lcPolicy mpParams 


-- | mkPFTPolicy is the minting policy for creating the approved merchan token.
--   When a merchant has one of a merchant approved token, they are authorized to spend/burn littercoin.
{-# INLINABLE mkMerchantTokenPolicy #-}
mkMerchantTokenPolicy :: MerchantTokenMintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkMerchantTokenPolicy params (MintPolicyRedeemer polarity _ _) ctx = 

    case polarity of
        True ->    traceIfFalse "MTP1" checkMintedAmount 
                && traceIfFalse "MTP2" signedByAdmin
                && traceIfFalse "MTP3" checkOwnerToken 
                
        False ->   traceIfFalse "MTP4" checkBurnedAmount 

  where
    tn :: Value.TokenName
    tn = mtMerchantTokenName params

    info :: TxInfo
    info = scriptContextTxInfo ctx  

    -- For now this is a single signature witnes, with future plans to make this multi-sig
    signedByAdmin :: Bool
    signedByAdmin =  txSignedBy info $ Address.unPaymentPubKeyHash (mtAdminPkh params)

    -- Check that there is only 1 token minted
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

    -- Check for Owner token for minting of merchant token
    checkOwnerToken :: Bool
    checkOwnerToken = validOutputs (minAda <> (mtOwnerTokenValue params)) (txInfoOutputs info)


    -- Check that there is only 1 token burned
    checkBurnedAmount :: Bool
    checkBurnedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == (-1)
        _               -> False
           

-- | Wrap the minting policy using the boilerplate template haskell code
merchantTokenPolicy :: MerchantTokenMintPolicyParams -> TScripts.MintingPolicy
merchantTokenPolicy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> TScripts.wrapMintingPolicy $ mkMerchantTokenPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
{-# INLINABLE merchantTokenCurSymbol #-}
merchantTokenCurSymbol :: MerchantTokenMintPolicyParams -> Value.CurrencySymbol
merchantTokenCurSymbol mpParams = scriptCurrencySymbol $ merchantTokenPolicy mpParams 


{-# INLINABLE merchantTokenValue #-}
merchantTokenValue :: Value.CurrencySymbol -> Value.TokenName -> Value.Value
merchantTokenValue cs' tn' = Value.singleton cs' tn' 1

{-# INLINABLE findDatum #-}
-- | Find the data corresponding to a data hash, if there is one
findDatum :: Scripts.DatumHash -> TxInfo -> Maybe Scripts.Datum
findDatum dHash TxInfo{txInfoData} = snd <$> find f txInfoData
  where
    f (dHash', _) = dHash' == dHash


-- | The LC validator is used only for minting, burning, adding and removing of Ada 
--   from the Littercoin smart contract.  
{-# INLINABLE mkLCValidator #-}
mkLCValidator :: LCValidatorParams -> LCDatum -> LCRedeemer -> ScriptContext -> Bool
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
        tn :: Value.TokenName
        tn = lcvLCTokenName params
        
        info :: TxInfo
        info = scriptContextTxInfo ctx  

        -- find the output datum
        outputDat :: LCDatum
        (_, outputDat) = case Contexts.getContinuingOutputs ctx of
            [o] -> case Tx.txOutDatumHash o of
                Nothing -> traceError "LCV14"                -- wrong output type
                Just h -> case findDatum h info of
                    Nothing -> traceError "LCV15"           -- datum not found
                    Just (Scripts.Datum d) ->  case PlutusTx.fromBuiltinData d of
                        Just ld' -> (o, ld')
                        Nothing  -> traceError "LCV16"       -- error decoding datum data
            _   -> traceError "LCV17"                        -- expected exactly one continuing output

        -- | Check that the Littercoin token name minted is equal to the amount in the redeemer
        checkAmountMint :: Integer -> Bool
        checkAmountMint q = case Value.flattenValue (txInfoMint info) of
            [(_, tn', amt)] -> tn' == tn && amt == q
            _               -> False

        -- | Check that the difference between LCAmount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumMint :: Integer -> Bool
        checkLCDatumMint q = ((lcAmount outputDat) - (lcAmount dat)) == q

        -- | Check that the Ada spent matches Littercoin minted and that the
        -- | tt token is also present
        checkValueAmountMint :: Bool
        checkValueAmountMint = validOutputs (adaBalance <> (lcvThreadTokenValue params)) (txInfoOutputs info)

            where     
                adaBalance = Ada.lovelaceValueOf (lcAdaAmount outputDat)

        -- | Check that the tx is signed by the admin 
        signedByAdmin :: Bool
        signedByAdmin =  txSignedBy info $ Address.unPaymentPubKeyHash (lcvAdminPkh params)  

        -- | Check that the littercoin token name burned is equal to the amount in the redeemer
        checkAmountBurn :: Integer -> Bool
        checkAmountBurn q = case Value.flattenValue (txInfoMint info) of
            [(_, tn', amt)] -> tn' == tn && amt == (negate q)
            _               -> False

        -- | Check that the difference between LCAmount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumBurn :: Integer -> Bool
        checkLCDatumBurn q = ((lcAmount dat) - (lcAmount outputDat)) == q &&
                             ((lcAdaAmount dat) - (lcAdaAmount outputDat) == q * ratio)
            where
                ratio :: Integer
                ratio = divide (lcAdaAmount dat) (lcAmount dat) -- TODO handle 0 lc amount condition


        -- | Check that the Ada spent matches Littercoin burned and that the
        -- | merch token is also present
        checkValueAmountBurn :: Bool
        checkValueAmountBurn = validOutputs (newAdaBalance <> (lcvThreadTokenValue params)) (txInfoOutputs info)

            where     
                newAdaBalance = Ada.lovelaceValueOf (lcAdaAmount outputDat)


        -- | Check that the difference between Ada amount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumAdd :: Integer -> Bool
        checkLCDatumAdd q = (lcAdaAmount outputDat) - (lcAdaAmount dat) == q


        -- | Check that the thread token is part of the outputs  
        --  TODO confirm that tt is locked at script address
        checkValueAmountAdd :: Bool
        checkValueAmountAdd = validOutputs (addAda <> tt) (txInfoOutputs info)

            where
                addAda :: Value.Value
                addAda = Ada.lovelaceValueOf (lcAdaAmount outputDat)

                tt :: Value.Value 
                tt = lcvThreadTokenValue params


        -- Check for Owner token required for minting
        checkOwnerToken :: Bool
        checkOwnerToken = validOutputs (minAda <> (lcvOwnerTokenValue params)) (txInfoOutputs info)


        -- Check for Merchant token if burning littercoin
        checkMerchantToken :: Bool
        checkMerchantToken = validOutputs (withdrawAda <> (lcvMerchantTokenValue params)) (txInfoOutputs info)

            where
                withdrawAda :: Value.Value
                withdrawAda = Ada.lovelaceValueOf ((lcAdaAmount dat) - (lcAdaAmount outputDat))


-- | Creating a wrapper around littercoin validator for 
--   performance improvements by not using a typed validator
{-# INLINABLE wrapLCValidator #-}
wrapLCValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapLCValidator params dat red ctx =
   check $ mkLCValidator (unsafeFromBuiltinData params) (unsafeFromBuiltinData dat) (unsafeFromBuiltinData red) (unsafeFromBuiltinData ctx)


untypedLCValidator :: BuiltinData -> Scripts.Validator
untypedLCValidator params = Scripts.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapLCValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
    

-- | We need a typedValidator for offchain mkTxConstraints, so 
-- created it using the untyped validator
typedLCValidator :: BuiltinData -> TScripts.TypedValidator TypeUtils.Any
typedLCValidator params =
  Validators.unsafeMkTypedValidator $ untypedLCValidator params


mkLCScript :: BuiltinData -> Scripts.Script
mkLCScript params = unValidatorScript $ untypedLCValidator params


lcValidator :: BuiltinData -> Scripts.Validator
lcValidator params = TScripts.validatorScript $ typedLCValidator params


lcHash :: BuiltinData -> Scripts.ValidatorHash
lcHash params = TScripts.validatorHash $ typedLCValidator params


untypedLCHash :: BuiltinData -> Scripts.ValidatorHash
untypedLCHash params = Scripts.validatorHash $ untypedLCValidator params


-- | Mint a unique thread and owner token 
mkThreadTokenPolicy :: ThreadTokenRedeemer -> ScriptContext -> Bool
mkThreadTokenPolicy (ThreadTokenRedeemer (Tx.TxOutRef refHash refIdx)) ctx = 
       traceIfFalse "TP1" checkTxIdx               -- Make sure does not exceed 256
    && traceIfFalse "TP1" txOutputSpent         -- UTxO not consumed
    && traceIfFalse "TP2" checkMintedAmount     -- wrong amount minted    
  
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownSymbol = Contexts.ownCurrencySymbol ctx
    minted = txInfoMint info
    threadToken = sha2_256 $ getTxId refHash <> intToBBS refIdx
    ownerToken = sha2_256 threadToken

    -- | check that refIdx < 256 and fail if not to mitigate 
    -- wrapping back to 0 due to word8 conversion
    checkTxIdx :: Bool
    checkTxIdx = refIdx < 256

    txOutputSpent :: Bool
    txOutputSpent = Contexts.spendsOutput info refHash refIdx

    checkMintedAmount :: Bool
    checkMintedAmount = minted == threadTokenValue ownSymbol (Value.TokenName threadToken) 
                                     <> threadTokenValue ownSymbol (Value.TokenName ownerToken)


{-# INLINABLE wrapThreadTokenPolicy #-}
wrapThreadTokenPolicy :: BuiltinData -> BuiltinData -> ()
wrapThreadTokenPolicy redeemer ctx =
   check $ mkThreadTokenPolicy (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx)


threadTokenPolicy :: TScripts.MintingPolicy
threadTokenPolicy = Scripts.mkMintingPolicyScript $
     $$(PlutusTx.compile [|| wrapThreadTokenPolicy ||])


{-# INLINABLE threadTokenCurSymbol #-}
threadTokenCurSymbol :: Value.CurrencySymbol
threadTokenCurSymbol = Contexts.scriptCurrencySymbol threadTokenPolicy


{-# INLINABLE threadTokenValue #-}
threadTokenValue :: Value.CurrencySymbol -> Value.TokenName -> Value.Value
threadTokenValue cs' tn' = Value.singleton cs' tn' 1
