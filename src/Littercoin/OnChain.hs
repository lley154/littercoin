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
    , lcValidator
    , lcPolicy
    , LCDatum(..)
    , minAda
    , threadTokenCurSymbol
    , threadTokenPolicy 
    , threadTokenValue
    , typedLCValidator
    )
where
    
import           Data.Aeson                             (FromJSON, ToJSON)
import           GHC.Generics                           (Generic)
import qualified Ledger.Ada                             as Ada (lovelaceValueOf)
import qualified Ledger.Address                         as Address (Address(..), PaymentPubKeyHash(..), toPubKeyHash)
import qualified Ledger.Value                           as Value (CurrencySymbol, flattenValue, singleton, TokenName(..), Value)
import           Littercoin.Types                       (MintPolicyRedeemer(..),
                                                        LCMintPolicyParams(..), LCRedeemer(..),
                                                        LCValidatorParams(..),
                                                        ThreadTokenRedeemer(..))
import qualified Plutus.Script.Utils.Typed              as Typed (Any, validatorScript)
import qualified Plutus.Script.Utils.V2.Scripts         as PSU.V2  (scriptCurrencySymbol, Validator, 
                                                        ValidatorHash) 
import qualified Plutus.Script.Utils.V2.Typed.Scripts   as PSU.V2 (mkUntypedMintingPolicy, TypedValidator)
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as ValidatorsV2 (unsafeMkTypedValidator, 
                                                        validatorHash)
import qualified Plutus.V2.Ledger.Api                   as PlutusV2 (CurrencySymbol, getDatum,  
                                                        MintingPolicy, mkMintingPolicyScript, mkValidatorScript, 
                                                        PubKeyHash(..), scriptContextTxInfo, TokenName(..), TxInfo,  
                                                        unsafeFromBuiltinData)
import qualified Plutus.V2.Ledger.Contexts              as ContextsV2 (getContinuingOutputs, ownCurrencySymbol, 
                                                        ScriptContext, spendsOutput, TxInfo, TxInInfo, txInfoMint, 
                                                        txInInfoResolved, txInfoInputs, txInfoOutputs, TxOut, txOutDatum, 
                                                        txOutAddress, txOutValue, txSignedBy)
import qualified Plutus.V2.Ledger.Tx                    as TxV2 (getTxId, OutputDatum(..), TxOut(..), 
                                                        TxOutRef(..))
import qualified PlutusTx                               (applyCode, compile, fromBuiltinData, liftCode, 
                                                        makeIsDataIndexed, makeLift)                       
import           PlutusTx.Prelude                       (Bool(..), BuiltinData, BuiltinByteString, check, consByteString, 
                                                        divide, emptyByteString, error, Integer, indexByteString, lengthOfByteString, 
                                                        Maybe(..), otherwise, quotient, remainder, sha2_256,
                                                        traceIfFalse, (*), (&&), ($), (<>), (==), (-), (+), (<), (!!))
import           Prelude                                (Show (..))



-------------------------------------------------
-- ON CHAIN CODE --
-------------------------------------------------

-- | Min Ada is the minimum amout of ada that needs to be included when 
--   transfering/minting a native asset so we can match a known
--   input value for the owner and merchant token transactions.
{-# INLINABLE minAda #-}
minAda :: Value.Value
minAda = Ada.lovelaceValueOf 5000000

-- | Create a BuitinByteString from an Integer
{-# INLINEABLE intToBBS #-}
intToBBS :: Integer -> BuiltinByteString
intToBBS y = consByteString (y + 48::Integer) emptyByteString -- 48 is ASCII code for '0'


-- | ActionDatum is used for actions to perform on the smart contract
data ActionDatum = ActionDatum
    {   adSequence          :: Integer
    ,   adAmount            :: Integer   -- either Ada or LC amount
    ,   adDestPaymentPkh    :: BuiltinByteString
    ,   adDestStakePkh      :: BuiltinByteString
    ,   adReturnPaymentPkh  :: BuiltinByteString
    ,   adReturnStakePkh    :: BuiltinByteString
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''ActionDatum [('ActionDatum, 0)]
PlutusTx.makeLift ''ActionDatum


-- | LCDatum is used to record the amount of Littercoin minted and the amount
--   of Ada locked at the smart contract.  This is then used during Littercoin
--   burning to payout the corresponding amount of Ada per Littercoin to the merchant.
data LCDatum = LCDatum
    {   lcAdaAmount             :: Integer                                         
    ,   lcAmount                :: Integer
    ,   lcReserve               :: Integer                                                                         
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''LCDatum [('LCDatum, 0)]
PlutusTx.makeLift ''LCDatum


-- Convert from a byte string to its hex (base16) representation. Example: [2, 14, 255] => "020eff"
{-# INLINEABLE encodeHex #-}
encodeHex :: BuiltinByteString -> BuiltinByteString
encodeHex input = go 0
  where
    len = lengthOfByteString input
    go :: Integer -> BuiltinByteString
    go i
      | i == len = emptyByteString
      | otherwise =
        consByteString (toChar $ byte `quotient` 16) $
          consByteString (toChar $ byte `remainder` 16) (go $ i + 1)
      where
        byte = indexByteString input i

        toChar :: Integer -> Integer
        toChar x
          -- 48 is ASCII code for '0'
          | x < 10 = x + 48
          -- 97 is ASCII code for 'a'
          -- x - 10 + 97 = x + 87
          | otherwise = x + 87


-- | Check to see if the thread token belongs to a value
{-# INLINABLE checkTTValue #-}
checkTTValue :: Value.Value -> Value.Value -> Bool
checkTTValue ttValue addrValue  = 
    let (buyCs, buyTn, buyAmt) = (Value.flattenValue ttValue)!!0
        valuesAtAddr = Value.flattenValue addrValue

        inspectValues :: [(Value.CurrencySymbol, Value.TokenName, Integer)] -> Bool
        inspectValues [] = False
        inspectValues ((cs, tn', amt):xs)
            | (cs == buyCs) && (tn' == buyTn) && (amt == buyAmt) = True
            | otherwise = inspectValues xs
    in inspectValues valuesAtAddr

-- | Find the thread token in the list of outputs, returning the scripts address 
--   of that utxo
{-# INLINABLE findTTOutput #-}
findTTOutput :: Value.Value -> [ContextsV2.TxOut] -> Maybe Address.Address
findTTOutput _ [] = Nothing
findTTOutput txVal (x:xs) 
    | checkTTValue txVal (ContextsV2.txOutValue x) = Just (ContextsV2.txOutAddress x)
    | otherwise = findTTOutput txVal xs


-- | Find the thread token in the list of inputs, returning the address 
--   of that utxo
{-# INLINABLE findTTAddrInputs #-}
findTTAddrInputs :: Value.Value -> [ContextsV2.TxInInfo] -> Maybe Address.Address
findTTAddrInputs _ [] = Nothing
findTTAddrInputs txVal (x:xs) = case findTTOutput txVal [ContextsV2.txInInfoResolved x] of
                                    (Just scriptAddr) -> Just scriptAddr  
                                    Nothing           ->  findTTAddrInputs txVal xs

 

{-# INLINABLE checkAddress' #-}
checkAddress' :: BuiltinByteString -> Address.Address -> Bool
checkAddress' pkh outAddr = case Address.toPubKeyHash outAddr of
                                Just (outPkh) -> (encodeHex (PlutusV2.getPubKeyHash outPkh)) == pkh 
                                Nothing -> False


-- | Check to see if the buy token is in the list of outputs locked at an address            
{-# INLINABLE validOutput' #-}
validOutput' :: Address.Address -> Value.Value -> [ContextsV2.TxOut] -> Bool
validOutput' _ _ [] = False
validOutput' scriptAddr txVal (x:xs)
    | (ContextsV2.txOutAddress x == scriptAddr) && (ContextsV2.txOutValue x == txVal) = True
    | otherwise = validOutput' scriptAddr txVal xs


-- | Check that the value is locked at an address for the provided outputs
{-# INLINABLE validOutput'' #-}
validOutput'' :: BuiltinByteString -> Value.Value -> [ContextsV2.TxOut] -> Bool
validOutput'' _ _ [] = False
validOutput'' pkh txVal (x:xs)
    | (checkAddress' pkh (ContextsV2.txOutAddress x)) && (ContextsV2.txOutValue x == txVal) = True
    | otherwise = validOutput'' pkh txVal xs


-- | Find a datum for a given value in the provided outputs
{-# INLINABLE getDatumOutput #-}
getDatumOutput :: Value.Value -> [ContextsV2.TxOut] -> Maybe TxV2.OutputDatum
getDatumOutput _ [] = Nothing
getDatumOutput txVal (x:xs)
    | (ContextsV2.txOutValue x == txVal) = Just (ContextsV2.txOutDatum x)
    | otherwise = getDatumOutput txVal xs


-- | Find a datum for a give value in the provided inputs
{-# INLINABLE getSequence #-}
getSequence :: TxV2.OutputDatum -> Maybe Integer
getSequence (TxV2.OutputDatum d) = case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d of
                                    Just (d') -> Just (adSequence d')
                                    Nothing  -> error ()    -- error decoding datum data
getSequence _ = error () -- expecting inline datum not datum hash or no datum


-- | Find a datum for a give value in the provided inputs and sequence number
{-# INLINABLE getDatumInput #-}
getDatumInput :: Value.Value -> Integer -> [ContextsV2.TxInInfo] -> Maybe TxV2.OutputDatum
getDatumInput _  _ [] = Nothing
getDatumInput txVal seqNum (x:xs) = case getDatumOutput txVal [ContextsV2.txInInfoResolved x] of
                                (Just outputDatum)  -> case getSequence outputDatum of
                                                        (Just seq)  -> if seq == seqNum then Just outputDatum
                                                                        else getDatumInput txVal seqNum xs
                                                        Nothing     -> getDatumInput txVal seqNum xs

                                Nothing             -> getDatumInput txVal seqNum xs



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
        MintLC seq    ->  traceIfFalse "LCV1" signedByAdmin
                  &&   (traceIfFalse "LCV2" $ checkLCDatumMint seq)
                  &&   (traceIfFalse "LCV3" $ checkOwnerToken seq)
                  &&   (traceIfFalse "LCV4" $ checkMintDestAddr seq)
                  &&   traceIfFalse "LCV5" checkThreadToken  

        BurnLC seq    ->  traceIfFalse "LCV7" signedByAdmin
                  &&   (traceIfFalse "LCV8" $ checkLCDatumBurn seq)
                  &&   (traceIfFalse "LCV9" $ checkBurnDestAddr seq)
                  &&   traceIfFalse "LCV10" checkThreadToken               
                    
        AddAda seq    ->  traceIfFalse "LCV12" signedByAdmin
                  &&   (traceIfFalse "LCV13" $ checkLCDatumAdd seq)  
                  &&   traceIfFalse "LCV14" checkThreadToken 

        SpendAction   -> traceIfFalse "LCV15" signedByAdmin
                  &&   traceIfFalse "LCV16" checkThreadToken

      where        
        lcTokenName :: PlutusV2.TokenName
        lcTokenName = lcvLCTokenName params
        
        info :: ContextsV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx  
        
        -- | Find the output datum
        outputDat :: LCDatum
        (_, outputDat) = case ContextsV2.getContinuingOutputs ctx of
            [o] -> case TxV2.txOutDatum o of
                TxV2.NoOutputDatum -> error ()       -- no datum present
                TxV2.OutputDatumHash _ -> error ()     -- expecting inline datum and not hash
                TxV2.OutputDatum d -> case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d of
                    Just d' -> (o, d')
                    Nothing  -> error ()       -- error decoding datum data
                    
            _   -> error ()                        -- expected exactly one continuing output
            
        scriptAddr :: Maybe Address.Address    
        scriptAddr = findTTAddrInputs (lcvThreadTokenValue params) (ContextsV2.txInfoInputs info)

        getAmount :: TxV2.OutputDatum -> Maybe Integer
        getAmount (TxV2.OutputDatum d) = case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d of
                                            Just (d') -> Just (adAmount d')
                                            Nothing  -> error ()     -- error decoding datum data
        getAmount _ = error () -- expecting inline datum not datum hash or no datum


        addAdaAmount :: Value.Value
        addAdaAmount = Ada.lovelaceValueOf ((lcAdaAmount outputDat) - (lcAdaAmount dat))

        withdrawAdaAmount :: Value.Value
        withdrawAdaAmount = Ada.lovelaceValueOf ((lcAdaAmount dat) - (lcAdaAmount outputDat))

        mintLCAmount :: Value.Value 
        mintLCAmount = Value.singleton (lcvLCTokenCurSymbol params) lcTokenName ((lcAmount outputDat) - (lcAmount dat))

        burnLCAmount :: Value.Value
        burnLCAmount = Value.singleton (lcvLCTokenCurSymbol params) lcTokenName ((lcAmount dat) - (lcAmount outputDat))

        getActionDatum :: Integer -> Maybe TxV2.OutputDatum
        getActionDatum seqNumber = getDatumInput (minAda <> lcvOwnerTokenValue params) seqNumber (ContextsV2.txInfoInputs info)

        -- | Check that the tx is signed by the admin 
        signedByAdmin :: Bool
        signedByAdmin =  ContextsV2.txSignedBy info $ Address.unPaymentPubKeyHash (lcvAdminPkh params)
          

        -- | Check that the difference between LCAmount in the output and the input datum
        --   matches the quantity indicated in the action Datum
        checkLCDatumMint :: Integer -> Bool
        checkLCDatumMint seqNum = 
            case getActionDatum seqNum of 
                (Just outDatum) -> case getAmount outDatum of
                    (Just lcAmt) ->   (((lcAmount outputDat) - (lcAmount dat)) == lcAmt )
                                   && (((lcReserve dat) - (lcReserve outputDat)) == lcAmt )                   
                    Nothing -> False
                Nothing -> False


        -- Check for Owner token required for minting
        checkOwnerToken :: Integer -> Bool
        checkOwnerToken seqNum = 
            case getActionDatum seqNum of 
                (Just outDatum) -> case getReturnPkh outDatum of
                    (Just returnPkh) -> validOutput'' returnPkh (minAda <> (lcvOwnerTokenValue params)) (ContextsV2.txInfoOutputs info)
                    Nothing -> False
                Nothing -> False

            where
                getReturnPkh :: TxV2.OutputDatum -> Maybe BuiltinByteString
                getReturnPkh (TxV2.OutputDatum d) = case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d of
                                                        Just (d') -> Just (adReturnPaymentPkh d')
                                                        Nothing  -> error ()     -- error decoding datum data
                getReturnPkh _ = error () -- expecting inline datum not datum hash or no datum


        -- Check minting destination address
        checkMintDestAddr :: Integer -> Bool
        checkMintDestAddr seqNum = 
            case getActionDatum seqNum of 
                (Just outDatum) -> case getDestPkh outDatum of
                    (Just destPkh) -> validOutput'' destPkh (minAda <> mintLCAmount) (ContextsV2.txInfoOutputs info)                  
                    Nothing -> False
                Nothing -> False
            where
                getDestPkh :: TxV2.OutputDatum -> Maybe BuiltinByteString
                getDestPkh (TxV2.OutputDatum d) = case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d of
                                                        Just (d') -> Just (adDestPaymentPkh d')
                                                        Nothing  -> error ()     -- error decoding datum data
                getDestPkh _ = error () -- expecting inline datum not datum hash or no datum

        -- | Check that the difference between LCAmount in the output and the input datum
        --   matches the quantity indicated in the redeemer
        checkLCDatumBurn :: Integer -> Bool
        checkLCDatumBurn seqNumber = 
            case getActionDatumBurn of 
                (Just outDatum) -> case getAmount outDatum of
                    (Just lcAmt) ->    (((lcAmount dat) - (lcAmount outputDat)) == lcAmt) 
                                    && (((lcReserve outputDat) - (lcReserve dat)) == lcAmt) 
                                    && ((lcAdaAmount dat) - (lcAdaAmount outputDat) == lcAmt * r)        
                    Nothing -> False
                Nothing -> False

            where
                getActionDatumBurn :: Maybe TxV2.OutputDatum
                getActionDatumBurn = getDatumInput (minAda <> burnLCAmount <> lcvMerchantTokenValue params) seqNumber (ContextsV2.txInfoInputs info)

                r :: Integer
                r = divide (lcAdaAmount dat) (lcAmount dat) -- TODO handle 0 lc amount condition

        -- Check burn destination address for Ada sent
        checkBurnDestAddr :: Integer -> Bool
        checkBurnDestAddr seqNumber = 
            case getActionDatumBurn of 
                (Just outDatum) -> case getDestPkh outDatum of
                    (Just destPkh) -> validOutput'' destPkh (withdrawAdaAmount <> (lcvMerchantTokenValue params)) (ContextsV2.txInfoOutputs info)       
                    Nothing -> False
                Nothing -> False
            where
                getActionDatumBurn :: Maybe TxV2.OutputDatum
                getActionDatumBurn = getDatumInput (minAda <> burnLCAmount <> lcvMerchantTokenValue params) seqNumber (ContextsV2.txInfoInputs info)

                getDestPkh :: TxV2.OutputDatum -> Maybe BuiltinByteString
                getDestPkh (TxV2.OutputDatum d) = case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d of
                                                        Just (d') -> Just (adDestPaymentPkh d')
                                                        Nothing  -> error ()     -- error decoding datum data
                getDestPkh _ = error () -- expecting inline datum not datum hash or no datum


        -- | Check that the difference between Ada amount in the output and the input datum
        --   matches the quantity indicated in the action datum
        checkLCDatumAdd :: Integer -> Bool
        checkLCDatumAdd seqNumber =        
            case getActionDatumAdd of 
                (Just outDatum) -> case getAmount outDatum of
                    (Just lcAmt) -> (lcAdaAmount outputDat) - (lcAdaAmount dat) == lcAmt            
                    Nothing -> False
                Nothing -> False
            where
                getActionDatumAdd :: Maybe TxV2.OutputDatum
                getActionDatumAdd = getDatumInput (addAdaAmount <> lcvDonationTokenValue params) seqNumber (ContextsV2.txInfoInputs info)


        -- | Check that the Ada added matches increase in the datum
        checkThreadToken :: Bool
        checkThreadToken = case scriptAddr of
                            (Just addr) -> validOutput' addr (newAdaBalance <> (lcvThreadTokenValue params) <> lcReserveAmount) (ContextsV2.txInfoOutputs info)
                            Nothing -> False
            where
                newAdaBalance :: Value.Value
                newAdaBalance = Ada.lovelaceValueOf (lcAdaAmount outputDat)

                lcReserveAmount :: Value.Value
                lcReserveAmount = Value.singleton (lcvLCTokenCurSymbol params) lcTokenName (lcReserve outputDat)


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

    checkMintedAmount :: Bool
    checkMintedAmount = minted == threadTokenValue ownSymbol (PlutusV2.TokenName threadToken) 


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



-- | The Littercoin minting policy is used to mint and burn littercoins according to the
--   following conditions set out in the policy.     
{-# INLINABLE mkLittercoinPolicy #-}
mkLittercoinPolicy :: LCMintPolicyParams -> MintPolicyRedeemer -> ContextsV2.ScriptContext -> Bool
mkLittercoinPolicy params (MintPolicyRedeemer polarity) ctx = 

    case polarity of
        True ->   traceIfFalse "LP1" checkMintedAmount
               && traceIfFalse "LP2" txOutputSpent
        False -> False  -- no burning allowed

    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx
        
        txOutputSpent :: Bool
        txOutputSpent = ContextsV2.spendsOutput info (TxV2.txOutRefId (lcTxOutRef params)) (TxV2.txOutRefIdx (lcTxOutRef params))
        
        ownSymbol = ContextsV2.ownCurrencySymbol ctx
        minted = ContextsV2.txInfoMint info

        checkMintedAmount :: Bool
        checkMintedAmount = minted == Value.singleton ownSymbol (lcTokenName params) (lcReserveAmt params)


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

