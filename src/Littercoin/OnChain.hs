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
--import           Littercoin.Utils                  (integerToBS)
import           Ledger                             (mkMintingPolicyScript, ScriptContext(..), scriptCurrencySymbol, 
                                                     TxInfo(..),  txSignedBy)
import qualified Ledger.Address as Address          (PaymentPubKeyHash(..))                                                     
import qualified Ledger.Typed.Scripts as Scripts    (MintingPolicy, wrapMintingPolicy)
import qualified Ledger.Value as Value              (CurrencySymbol, flattenValue, TokenName(..))
import qualified PlutusTx                           (applyCode, compile, liftCode)
import           PlutusTx.Prelude                   (Bool(..), traceIfFalse, (&&), (==), ($), (<=), (>=))


------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

-- | The mkPolicy is the minting policy validator for minting and burning
--   of littercoin tokens.  It is a parameterized validator so each policy
--   id is unqiue based on the admin pkh provided.

{-# INLINABLE mkPolicy #-}
mkPolicy :: MintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkPolicy params (MintPolicyRedeemer polarity) ctx = 

    case polarity of
        True ->    traceIfFalse "mkPolicy mint: invalid admin signature" signedByAdmin
                && traceIfFalse "mkPolicy mint: invalid token amount" checkMintedAmount
                
        False ->  traceIfFalse "mkPolicy burn: invalid token amount" checkBurnedAmount
                -- check for merchant auth NFT

  where

    tn :: Value.TokenName
    tn = mpTokenName params

    info :: TxInfo
    info = scriptContextTxInfo ctx  

    -- For now this is a single signature witnes, with future plans to make this multi-sig
    signedByAdmin :: Bool
    signedByAdmin =  txSignedBy info $ Address.unPaymentPubKeyHash (mpAdminPkh params)

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
policy :: MintPolicyParams -> Scripts.MintingPolicy
policy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> Scripts.wrapMintingPolicy $ mkPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
curSymbol :: MintPolicyParams -> Value.CurrencySymbol
curSymbol mpParams = scriptCurrencySymbol $ policy mpParams 

