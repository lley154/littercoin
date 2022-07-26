{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Littercoin.OnChain 
    (
      lcCurSymbol
    , lcPolicy
    , nftCurSymbol
    , nftPolicy
    , nftTokenValue
    ) where

import           Littercoin.Types                   (LCMintPolicyParams(..), NFTMintPolicyParams(..), MintPolicyRedeemer(..))
import           Ledger                             (mkMintingPolicyScript, ScriptContext(..), scriptCurrencySymbol, 
                                                     TxInfo(..),  txSignedBy)
import qualified Ledger.Address as Address          (PaymentPubKeyHash(..))                                                     
import qualified Ledger.Typed.Scripts as Scripts    (MintingPolicy, wrapMintingPolicy)
import qualified Ledger.Value as Value              (CurrencySymbol, flattenValue, singleton, TokenName(..), Value)
import qualified PlutusTx                           (applyCode, compile, liftCode)
import           PlutusTx.Prelude                   (Bool(..), traceIfFalse, (&&), (==), ($), (<=), (>=))


------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

-- | The mkLittercoinPolicy is the minting policy validator for minting and burning
--   of littercoin tokens.  It is a parameterized validator so each policy
--   id is unqiue based on the admin pkh provided.

{-# INLINABLE mkLittercoinPolicy #-}
mkLittercoinPolicy :: LCMintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkLittercoinPolicy params (MintPolicyRedeemer polarity) ctx = 

    case polarity of
        True ->    traceIfFalse "mkPolicy mint: invalid admin signature" signedByAdmin
                && traceIfFalse "mkPolicy mint: invalid token amount" checkMintedAmount
                
        False ->  traceIfFalse "mkPolicy burn: invalid token amount" checkBurnedAmount
                -- check for merchant auth NFT

  where

    tn :: Value.TokenName
    tn = lcTokenName params

    info :: TxInfo
    info = scriptContextTxInfo ctx  

    -- For now this is a single signature witnes, with future plans to make this multi-sig
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

    -- Check for NFT Auth token for burning
    --  

-- | Wrap the minting policy using the boilerplate template haskell code
lcPolicy :: LCMintPolicyParams -> Scripts.MintingPolicy
lcPolicy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> Scripts.wrapMintingPolicy $ mkLittercoinPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
lcCurSymbol :: LCMintPolicyParams -> Value.CurrencySymbol
lcCurSymbol mpParams = scriptCurrencySymbol $ lcPolicy mpParams 


{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: NFTMintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkNFTPolicy params (MintPolicyRedeemer polarity) ctx = 

    case polarity of
        True ->    traceIfFalse "mkPolicy: wrong amount minted" checkMintedAmount
                && traceIfFalse "mkPolicy: invalid admin signature" signedByAdmin
                
        False ->   traceIfFalse "mkPolicy: wrong amount burned" checkBurnedAmount

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
nftPolicy :: NFTMintPolicyParams -> Scripts.MintingPolicy
nftPolicy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> Scripts.wrapMintingPolicy $ mkNFTPolicy mpParams' ||])
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
