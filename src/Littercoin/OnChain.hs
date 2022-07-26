{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Littercoin.OnChain 
    (
      curSymbol
    , policy
    ) where

import           Littercoin.Types                  (MintPolicyParams(..), MintPolicyRedeemer(..))
import           Littercoin.Utils                  (integerToBS)
import           Ledger                             (mkMintingPolicyScript, ScriptContext(..), scriptCurrencySymbol, 
                                                     txInInfoOutRef, TxInfo(..), TxOutRef(..), txSignedBy)
import qualified Ledger.Address as Address          (PaymentPubKeyHash(..))                                                     
import qualified Ledger.Typed.Scripts as Scripts    (MintingPolicy, wrapMintingPolicy)
import qualified Ledger.Value as Value              (CurrencySymbol, flattenValue, TokenName(..))
import qualified PlutusTx                           (applyCode, compile, liftCode)
import           PlutusTx.Prelude                   (any, Bool(..), sha2_256, traceIfFalse, (&&), (==), ($), (<>))


------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

-- | The mkPolicy is the minting policy validator for minting and burning
--   of carbon credit tokens.  It is a parameterized validator so each policy
--   id is unqiue based on the provided inputs.  In particular, an unspent
--   UTXO must be provided as part of the MintPolicyParams and this can only
--   be valid once.  After it is spent, it cannot be spent again so
--   the minting policy will fail if that UTXO is re-used because it cannot be
--   a valid input.

{-# INLINABLE mkPolicy #-}
mkPolicy :: MintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkPolicy params (MintPolicyRedeemer polarity) ctx = 

    case polarity of
        True ->    traceIfFalse "mkPolicy: UTxO not consumed"   hasUTxO
                && traceIfFalse "mkPolicy: wrong amount minted" checkMintedAmount
                && traceIfFalse "mkPolicy: invalid admin signature" signedByAdmin
                && traceIfFalse "mkPolicy: invalid NFT meta data" validNFTParams
                
        False ->   traceIfFalse "mkPolicy: wrong amount burned" checkBurnedAmount
                && traceIfFalse "mkPolicy: invalid NFT meta data" validNFTParams


  where
    oref :: TxOutRef
    oref = mpOref params

    tn :: Value.TokenName
    tn = mpTokenName params

    info :: TxInfo
    info = scriptContextTxInfo ctx  

    -- Verify that the UTXO in the mint params is part of the tx inputs
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    -- For now this is a single signature witnes, with future plans to make this multi-sig
    signedByAdmin :: Bool
    signedByAdmin =  txSignedBy info $ Address.unPaymentPubKeyHash (mpAdminPkh params)

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

    -- Validate that the hash of the minting params is the same
    -- as the token name that is being minted or burned
    validNFTParams :: Bool
    validNFTParams = tn == (Value.TokenName $ sha2_256 tn'')
        where
            tn'' =  (mpAddress params) <> 
                    (integerToBS $ mpLat params) <> 
                    (integerToBS $ mpLong params) <> 
                    (mpCategory params) <> 
                    (mpMethod params) <> 
                    (integerToBS $ mpCO2Qty params)
           

-- | Wrap the minting policy using the boilerplate template haskell code
policy :: MintPolicyParams -> Scripts.MintingPolicy
policy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> Scripts.wrapMintingPolicy $ mkPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
curSymbol :: MintPolicyParams -> Value.CurrencySymbol
curSymbol mpParams = scriptCurrencySymbol $ policy mpParams 

