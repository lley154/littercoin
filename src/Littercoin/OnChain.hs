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
    , nftCurSymbol
    , nftPolicy
    , nftTokenValue
    , threadTokenCurSymbol
    , threadTokenPolicy 
    , threadTokenValue
    , typedLCValidator
    )
where
    
import           Data.Aeson                           (FromJSON, ToJSON)
import           GHC.Generics                         (Generic)
import qualified Ledger.Ada                           as Ada (lovelaceValueOf)
import qualified Ledger.Address                       as Address (PaymentPubKeyHash(..))
import qualified Ledger.Value                         as Value (CurrencySymbol, flattenValue, singleton, Value)
import qualified Plutus.Script.Utils.Typed            as Typed (Any, validatorScript)
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2  (scriptCurrencySymbol, Validator, ValidatorHash) 
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2 (mkUntypedMintingPolicy, TypedValidator)
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as ValidatorsV2 (unsafeMkTypedValidator, validatorHash)
import qualified Plutus.V2.Ledger.Api                 as PlutusV2 (CurrencySymbol, getDatum,  MintingPolicy, mkMintingPolicyScript, mkValidatorScript, Script, scriptContextTxInfo, TokenName(..), TxInfo, txInfoMint, txInfoOutputs, unsafeFromBuiltinData, unValidatorScript )
import qualified Plutus.V2.Ledger.Contexts            as ContextsV2 (getContinuingOutputs, ownCurrencySymbol, ScriptContext, spendsOutput, TxInfo, txInfoMint, txInfoOutputs, TxOut, txOutValue, txSignedBy )
import qualified Plutus.V2.Ledger.Tx                  as TxV2 (getTxId, OutputDatum(..), TxOut(..), TxOutRef(..))
import qualified PlutusTx                             (applyCode, compile, fromBuiltinData, liftCode, makeIsDataIndexed, makeLift)                       
import           PlutusTx.Prelude                     as P hiding
                                                      (unless, (.))
import           Prelude                              (Show (..))
import           Littercoin.Types                     (MintPolicyRedeemer(..), 
                                                       LCMintPolicyParams(..),
                                                       LCRedeemer(..),
                                                       LCValidatorParams(..),
                                                       NFTMintPolicyParams(..),
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
intToBBS y = consByteString (y + 48::Integer) emptyByteString -- 48 is ASCII code for '0'


-- | LCDatum is used to record the amount of Littercoin minted and the amount
--   of Ada locked at the smart contract.  This is then used during Littercoin
--   burning to payout the corresponding amount of Ada per Littercoin to the merchant.
data LCDatum = LCDatum
    {   adaAmount           :: Integer                                         
    ,   lcAmount            :: Integer                                                                         
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
mkLittercoinPolicy :: LCMintPolicyParams -> MintPolicyRedeemer -> ContextsV2.ScriptContext -> Bool
mkLittercoinPolicy params (MintPolicyRedeemer polarity withdrawAmount) ctx = 

    case polarity of
        True ->   traceIfFalse "LP1" signedByAdmin 
               && traceIfFalse "LP2" checkMintedAmount 
               -- TODO check for threadtoken
                
        False ->  traceIfFalse "LP3" checkBurnedAmount 
               && traceIfFalse "LP4" checkNFTValue -- check for merchant NFT

  where

    tn :: PlutusV2.TokenName
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
    checkNFTValue = validOutputs (withdrawAda P.<> (lcNFTTokenValue params)) (PlutusV2.txInfoOutputs info)

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
    tn :: PlutusV2.TokenName
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
nftTokenValue :: Value.CurrencySymbol -> PlutusV2.TokenName -> Value.Value
nftTokenValue cs' tn' = Value.singleton cs' tn' 1



-- | Mint a unique NFT representing a littercoin validator thread token
mkThreadTokenPolicy :: ThreadTokenRedeemer -> ContextsV2.ScriptContext -> Bool
mkThreadTokenPolicy (ThreadTokenRedeemer (TxV2.TxOutRef refHash refIdx)) ctx = 
    traceIfFalse "TP1" txOutputSpent            -- UTxO not consumed
    && traceIfFalse "TP2" checkMintedAmount     -- wrong amount minted  
    -- TODO check for admin pkh  
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
    threadToken = sha2_256 $ TxV2.getTxId refHash P.<> intToBBS refIdx


    checkMintedAmount :: Bool
    checkMintedAmount = minted == threadTokenValue ownSymbol (PlutusV2.TokenName threadToken) 


-- | Wrap the minting policy using the boilerplate template haskell code
threadTokenPolicy :: PlutusV2.MintingPolicy
threadTokenPolicy = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = PSU.V2.mkUntypedMintingPolicy mkThreadTokenPolicy 


{-


{-# INLINABLE wrapThreadTokenPolicy #-}
wrapThreadTokenPolicy :: BuiltinData -> BuiltinData -> ()
wrapThreadTokenPolicy redeemer ctx =
   check $ mkThreadTokenPolicy (PlutusTx.unsafeFromBuiltinData redeemer) (PlutusTx.unsafeFromBuiltinData ctx)


threadTokenPolicy :: PlutusV2.MintingPolicy
threadTokenPolicy = PlutusV2.mkMintingPolicyScript $
     $$(PlutusTx.compile [|| wrapThreadTokenPolicy ||])


-}

{-# INLINABLE threadTokenCurSymbol #-}
threadTokenCurSymbol :: Value.CurrencySymbol
threadTokenCurSymbol = PSU.V2.scriptCurrencySymbol threadTokenPolicy


{-# INLINABLE threadTokenValue #-}
threadTokenValue :: Value.CurrencySymbol -> PlutusV2.TokenName -> Value.Value
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
        tn :: PlutusV2.TokenName
        tn = lcvLCTokenName params
        
        info :: ContextsV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx  
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
        checkValueAmountBurn = validOutputs (newAdaBalance P.<> (lcvThreadTokenValue params)) (ContextsV2.txInfoOutputs info)
            where     
                newAdaBalance = Ada.lovelaceValueOf (adaAmount outputDat)
                
        -- | Check that the difference between Ada amount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumAdd :: Integer -> Bool
        checkLCDatumAdd q = (adaAmount outputDat) - (adaAmount dat) == q
        
        -- | Check that the Ada added matches increase in the datum
        checkValueAmountAdd :: Bool
        checkValueAmountAdd = validOutputs (addAda P.<> tt) (ContextsV2.txInfoOutputs info)
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
