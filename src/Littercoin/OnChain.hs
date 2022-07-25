{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Littercoin.OnChain
  ( --curSymbol
   policy
  )
where


import qualified Ledger
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Ledger.Value                   as Value
import           Littercoin.Types               (MintPolicyRedeemer(..), MintPolicyParams(..))
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as MP
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V2.Ledger.Contexts      as ContextV2
import qualified PlutusTx                       (applyCode, compile, liftCode)
import           PlutusTx.Prelude               (Bool(..), traceIfFalse, (==), (&&), (>=),($))


{-
   The policy
-}

{-# INLINABLE mkPolicy #-}
mkPolicy :: MintPolicyParams -> MintPolicyRedeemer -> PlutusV2.ScriptContext -> Bool
mkPolicy params (MintPolicyRedeemer polarity) ctx = 

   if polarity then
      traceIfFalse "mkPolicy: invalid admin signature" signedByAdmin
      && traceIfFalse "mkPolicy: invalid NFT params" validTokenName
   else            
      traceIfFalse "mkPolicy: invalid NFT params" validTokenName

   where
      pkh :: PlutusV2.PubKeyHash
      pkh = mpAdminPkh params

      tn :: PlutusV2.TokenName
      tn = mpTokenName params

      info :: PlutusV2.TxInfo
      info = PlutusV2.scriptContextTxInfo ctx  

      signedByAdmin :: Bool
      signedByAdmin =  ContextV2.txSignedBy info pkh

      -- Validate the correct token name that is being minted or burned
      validTokenName :: Bool
      validTokenName = case Value.flattenValue (PlutusV2.txInfoMint info) of
         [(cs, tn', amt)] -> cs == ContextV2.ownCurrencySymbol ctx && tn' == tn && amt >= 1
         _                -> False
 

{-
    As a Minting Policy
-}


-- | Wrap the minting policy using the boilerplate template haskell code
policy :: MintPolicyParams -> Scripts.MintingPolicy
policy mp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mp' -> MP.mkUntypedMintingPolicy $ mkPolicy mp' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
curSymbol :: MintPolicyParams -> Value.CurrencySymbol
curSymbol mpParams = Ledger.scriptCurrencySymbol $ policy mpParams 