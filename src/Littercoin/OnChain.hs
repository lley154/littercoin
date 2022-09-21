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
    --, lcCurSymbol
    --, lcHash
    --, lcPolicy
    --, lcValidator
    , LCDatum(..)
    , minAda
    --, nftCurSymbol
    --, nftPolicy
    --, nftTokenValue
    --, threadTokenCurSymbol
    --, threadTokenPolicy 
    --, threadTokenValue
    --, typedLCValidator
    ) where

import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           Littercoin.Types                   (LCMintPolicyParams(..), LCRedeemer(..), LCValidatorParams(..), NFTMintPolicyParams(..), MintPolicyRedeemer(..), ThreadTokenRedeemer(..))
--import           Ledger                             (mkMintingPolicyScript, ScriptContext(..), scriptCurrencySymbol, 
--                                                     TxInfo(..),  txSignedBy, TxId(getTxId ))
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import qualified Ledger.Address as Address          (PaymentPubKeyHash(..))
import qualified Plutus.V2.Ledger.Contexts as Contexts        (getContinuingOutputs, ownCurrencySymbol, ScriptContext(..), spendsOutput, txSignedBy, TxInfo(..), TxOut(..))
import qualified Plutus.Script.Utils.V2.Scripts as UScripts     (scriptCurrencySymbol, validatorHash, ValidatorHash)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as UTScripts (mkUntypedMintingPolicy, validatorHash)
import qualified Ledger.Scripts as Scripts          (Datum(..), DatumHash, mkMintingPolicyScript, mkValidatorScript, Script, Validator, ValidatorHash, validatorHash)                                                  
import qualified Ledger.Tx as Tx                    (TxOut(..), TxOutRef(..))
import qualified Ledger.Typed.Scripts.Validators as Validators (unsafeMkTypedValidator)
import qualified Ledger.Typed.TypeUtils as TypeUtils (Any)
import qualified Ledger.Typed.Scripts as TScripts   (MintingPolicy, TypedValidator, validatorScript, validatorHash)
import qualified Ledger.Value as Value              (CurrencySymbol, flattenValue, singleton, TokenName(..), Value)
--import           Plutus.V1.Ledger.Api as Ledger     (unsafeFromBuiltinData, unValidatorScript)
import qualified Plutus.V2.Ledger.Api as Ledger     (mkMintingPolicyScript)
import qualified PlutusTx                           (applyCode, compile, fromBuiltinData, liftCode, makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, BuiltinData, check, consByteString, divide, emptyByteString, find, Integer, Maybe(..), negate, otherwise, snd, sha2_256, traceIfFalse, traceError, (&&), (==), ($), (<=), (>=), (<>), (<$>), (-), (*), (+))
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
    {   adaAmount           :: !Integer                                         
    ,   lcAmount            :: !Integer                                                                          -- 8
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''LCDatum [('LCDatum, 0)]
PlutusTx.makeLift ''LCDatum



-- | Check that the NFT value is in the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Value.Value -> [Contexts.TxOut] -> Bool
validOutputs _ [] = False
validOutputs txVal (x:xs)
    | Contexts.txOutValue x == txVal = True
    | otherwise = validOutputs txVal xs
                             

-- | The mkLittercoinPolicy is the minting policy validator for minting and burning
--   of littercoin tokens.  It is a parameterized validator so each policy
--   id is unqiue based on the admin pkh provided.

{-# INLINABLE mkLittercoinPolicy #-}
mkLittercoinPolicy :: LCMintPolicyParams -> MintPolicyRedeemer -> Contexts.ScriptContext -> Bool
mkLittercoinPolicy params (MintPolicyRedeemer polarity withdrawAmount) ctx = 

    case polarity of
        True ->    traceIfFalse "LP1" signedByAdmin 
                && traceIfFalse "LP2" checkMintedAmount 
                
        False ->  traceIfFalse "LP3" checkBurnedAmount 
                && traceIfFalse "LP4" checkNFTValue -- check for merchant NFT

  where

    tn :: Value.TokenName
    tn = lcTokenName params

    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx  

    -- For now this is a single signature witnes, with future plans to make this multi-sig
    signedByAdmin :: Bool
    signedByAdmin =  Contexts.txSignedBy info $ Address.unPaymentPubKeyHash (lcAdminPkh params)

    -- Check that the token name minted is greater than 1
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (Contexts.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt >= 1
        _               -> False

    -- Check that the token name burned is less than -1
    checkBurnedAmount :: Bool
    checkBurnedAmount = case Value.flattenValue (Contexts.txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt <= (-1)
        _               -> False

    -- Check for NFT Merchant token if burning littercoin
    -- TODO, need to determine Ada at address or split out NFT validation
    checkNFTValue :: Bool
    checkNFTValue = validOutputs (withdrawAda <> (lcNFTTokenValue params)) (Contexts.txInfoOutputs info)

        where
            withdrawAda :: Value.Value
            withdrawAda = Ada.lovelaceValueOf (withdrawAmount)

-- | Wrap the minting policy using the boilerplate template haskell code
lcPolicy :: LCMintPolicyParams -> TScripts.MintingPolicy
lcPolicy mpParams = Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> UTScripts.mkUntypedMintingPolicy $ mkLittercoinPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
lcCurSymbol :: LCMintPolicyParams -> Value.CurrencySymbol
lcCurSymbol mpParams = UScripts.scriptCurrencySymbol $ lcPolicy mpParams 


{-

-- | mkPFTPolicy is the minting policy for creating the approved merchant NFT.
--   When a merchant has one of a merchant approved NFT, they are authorized to spend/burn littercoin.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: NFTMintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkNFTPolicy params (MintPolicyRedeemer polarity _) ctx = 

    case polarity of
        True ->    traceIfFalse "NFTP1" checkMintedAmount 
                && traceIfFalse "NFTP2" signedByAdmin 
                
        False ->   traceIfFalse "NFTP3" checkBurnedAmount 

  where
    tn :: Value.TokenName
    tn = nftTokenName params

    info :: TxInfo
    info = scriptContextTxInfo ctx  

    -- For now this is a single signature witnes, with future plans to make this multi-sig
    signedByAdmin :: Bool
    signedByAdmin =  txSignedBy info $ Address.unPaymentPubKeyHash (nftAdminPkh params)

    -- Check that there is only 1 token minted
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

    -- Check that there is only 1 token burned
    checkBurnedAmount :: Bool
    checkBurnedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == (-1)
        _               -> False
           

-- | Wrap the minting policy using the boilerplate template haskell code
nftPolicy :: NFTMintPolicyParams -> TScripts.MintingPolicy
nftPolicy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> TScripts.wrapMintingPolicy $ mkNFTPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
{-# INLINABLE nftCurSymbol #-}
nftCurSymbol :: NFTMintPolicyParams -> Value.CurrencySymbol
nftCurSymbol mpParams = scriptCurrencySymbol $ nftPolicy mpParams 


{-# INLINABLE nftTokenValue #-}
nftTokenValue :: Value.CurrencySymbol -> Value.TokenName -> Value.Value
nftTokenValue cs' tn' = Value.singleton cs' tn' 1

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

        BurnLC qty -> (traceIfFalse "LCV4" $ checkAmountBurn qty)           
                  &&  (traceIfFalse "LCV5" $ checkLCDatumBurn qty)
                  &&   traceIfFalse "LCV7" checkValueAmountBurn                 
                    
        AddAda qty -> (traceIfFalse "LCV8" $ checkLCDatumAdd qty)  
                 &&    traceIfFalse "LCV9" checkValueAmountAdd 


      where        
        tn :: Value.TokenName
        tn = lcvLCTokenName params
        
        info :: TxInfo
        info = scriptContextTxInfo ctx  

        -- find the output datum
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

        -- | Check that the Littercoin token name minted is equal to the amount in the redeemer
        checkAmountMint :: Integer -> Bool
        checkAmountMint q = case Value.flattenValue (txInfoMint info) of
            [(_, tn', amt)] -> tn' == tn && amt == q
            _               -> False

        -- | Check that the difference between LCAmount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumMint :: Integer -> Bool
        checkLCDatumMint q = ((lcAmount outputDat) - (lcAmount dat)) == q

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
                             ((adaAmount dat) - (adaAmount outputDat) == q * ratio)
            where
                ratio :: Integer
                ratio = divide (adaAmount dat) (lcAmount dat) -- TODO handle 0 lc amount condition


        -- | Check that the Ada spent matches Littercoin burned and that the
        -- | merch NFT token is also present
        checkValueAmountBurn :: Bool
        checkValueAmountBurn = validOutputs (newAdaBalance <> (lcvThreadTokenValue params)) (txInfoOutputs info)

            where     
                newAdaBalance = Ada.lovelaceValueOf (adaAmount outputDat)


        -- | Check that the difference between Ada amount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumAdd :: Integer -> Bool
        checkLCDatumAdd q = (adaAmount outputDat) - (adaAmount dat) == q


        -- | Check that the Ada added matches increase in the datum
        checkValueAmountAdd :: Bool
        checkValueAmountAdd = validOutputs (addAda <> tt) (txInfoOutputs info)

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


-- | Mint a unique NFT representing a littercoin validator thread token
mkThreadTokenPolicy :: ThreadTokenRedeemer -> ScriptContext -> Bool
mkThreadTokenPolicy (ThreadTokenRedeemer (Tx.TxOutRef refHash refIdx)) ctx = 
    traceIfFalse "TP1" txOutputSpent            -- UTxO not consumed
    && traceIfFalse "TP2" checkMintedAmount     -- wrong amount minted    
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- True if the pending transaction spends the output
    -- identified by @(refHash, refIdx)@
    -- TODO -- check that refIdx < 256 and fail if not to mitigate 
    -- wrapping back to 0 due to word8 conversion
    txOutputSpent = Contexts.spendsOutput info refHash refIdx
    ownSymbol = Contexts.ownCurrencySymbol ctx
    minted = txInfoMint info
    threadToken = sha2_256 $ getTxId refHash <> intToBBS refIdx

    checkMintedAmount :: Bool
    checkMintedAmount = minted == threadTokenValue ownSymbol (Value.TokenName threadToken) 


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

-}