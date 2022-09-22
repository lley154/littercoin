{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
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
import           Data.Text                            (Text)
import qualified Data.Map                             as Map
import qualified Ledger.Ada                           as Ada
import qualified Ledger.Address                       as Address
import qualified Ledger.Value                         as Value
import qualified Ledger.Constraints                   as Constraints
import qualified Plutus.Contract                      as Contract
import qualified Plutus.Contract.Request              as Request
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as Contexts
import qualified Plutus.V2.Ledger.Tx                  as Tx
import qualified PlutusTx                             
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       Show (..), String, print, (.),
                                                       Either(..), return)
import           Littercoin.Types                     (MintPolicyRedeemer(..), 
                                                       LCMintPolicyParams(..),
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


-- | Check that the NFT value is in the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Value.Value -> [Contexts.TxOut] -> Bool
validOutputs _ [] = False
validOutputs txVal (x:xs)
    | Contexts.txOutValue x == txVal = True
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
    signedByAdmin =  Contexts.txSignedBy info $ Address.unPaymentPubKeyHash (lcAdminPkh params)


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
mkNFTPolicy :: NFTMintPolicyParams -> MintPolicyRedeemer -> Contexts.ScriptContext -> Bool
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
    signedByAdmin =  Contexts.txSignedBy info $ Address.unPaymentPubKeyHash (nftAdminPkh params)

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
mkThreadTokenPolicy :: ThreadTokenRedeemer -> Contexts.ScriptContext -> Bool
mkThreadTokenPolicy (ThreadTokenRedeemer (Tx.TxOutRef refHash refIdx)) ctx = 
    traceIfFalse "TP1" txOutputSpent            -- UTxO not consumed
    && traceIfFalse "TP2" checkMintedAmount     -- wrong amount minted    
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    -- True if the pending transaction spends the output
    -- identified by @(refHash, refIdx)@
    -- TODO -- check that refIdx < 256 and fail if not to mitigate 
    -- wrapping back to 0 due to word8 conversion
    txOutputSpent = Contexts.spendsOutput info refHash refIdx
    ownSymbol = Contexts.ownCurrencySymbol ctx
    minted = Contexts.txInfoMint info
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

-------------------------------------------------
-- OFF CHAIN CODE --
-------------------------------------------------

-- | TokenSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mintLC" (Value.TokenName, TokenParams)
                   --Contract..\/ Contract.Endpoint "addAdaContract" (Value.TokenName, TokenParams)
                   --Contract..\/ Contract.Endpoint "burnLC" (Value.TokenName, TokenParams)
                   --Contract..\/ Contract.Endpoint "mintNFT" TokenParams
                   --Contract..\/ Contract.Endpoint "burnNFT" TokenParams



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
            Left _          -> Contract.throwError "findLCValidator: datum missing"
            Right (Scripts.Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> Contract.throwError "findLCValidator: datum has wrong type"
                Just d@LCDatum{} -> return (oref, o, d)
        _           -> Contract.throwError "findLCValidator: utxo not found"



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
        redLC = Scripts.Redeemer $ PlutusTx.toBuiltinData $ MintLC (tpQty tp)
        datLC = PlutusTx.toBuiltinData lcDatum


    ownPkh <- Request.ownPaymentPubKeyHash
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
                lookups = --Constraints.typedValidatorLookups (typedLCValidator $ PlutusTx.toBuiltinData lcParams) Haskell.<> 
                          --Constraints.otherScript (lcValidator $ PlutusTx.toBuiltinData lcParams) Haskell.<> 
                          --Constraints.unspentOutputs (Map.singleton orefLC oLC) Haskell.<> 
                          Constraints.plutusV2MintingPolicy (lcPolicy mintParams) <> 
                          Constraints.unspentOutputs utxos
                tx      = --Constraints.mustPayToTheScript datLC (Ada.lovelaceValueOf (adaAmount lcd) Haskell.<> ttVal) Haskell.<> 
                          --Constraints.mustSpendScriptOutput orefLC redLC Haskell.<>
                          Constraints.mustMintValueWithRedeemer red val <> 
                          Constraints.mustSpendPubKeyOutput oref <>
                          Constraints.mustBeSignedBy ownPkh

            utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
            let adjustedUtx = Constraints.adjustUnbalancedTx utx
            Request.submitTxConfirmed adjustedUtx
            Contract.logInfo $ "mintLCToken: tx submitted successfully= " ++ show adjustedUtx

-}



