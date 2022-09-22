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

module Littercoin.OnChain
  ( 
  )
where

import           Cardano.Api                          (PlutusScript,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
--import           GHC.Generics                         (Generic)
import qualified Ledger.Address as Address            (PaymentPubKeyHash(..))
import qualified Ledger.Typed.Scripts                 as Scripts
import           Ledger.Value                         as Value
--import           Playground.Contract as Playground    (ToSchema)
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import           Plutus.V2.Ledger.Contexts            (ownCurrencySymbol, txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       Show (..), print, (.))
import           Littercoin.Types




{-# INLINABLE mkLittercoinPolicy #-}
mkLittercoinPolicy :: LCMintPolicyParams -> MintPolicyRedeemer -> PlutusV2.ScriptContext -> Bool
mkLittercoinPolicy params (MintPolicyRedeemer polarity withdrawAmount) ctx = 

    case polarity of
        True ->   traceIfFalse "LP1" signedByAdmin 
               && traceIfFalse "LP2" checkMintedAmount 
                
        False ->  traceIfFalse "LP3" checkBurnedAmount 
               -- && traceIfFalse "LP4" checkNFTValue -- check for merchant NFT

  where

    tn :: PlutusV2.TokenName
    tn = lcTokenName params

    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx  

    -- For now this is a single signature witnes, with future plans to make this multi-sig
    signedByAdmin :: Bool
    signedByAdmin =  txSignedBy info $ Address.unPaymentPubKeyHash (lcAdminPkh params)


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
        
{-
    -- Check for NFT Merchant token if burning littercoin
    -- TODO, need to determine Ada at address or split out NFT validation
    checkNFTValue :: Bool
    checkNFTValue = validOutputs (withdrawAda <> (lcNFTTokenValue params)) (Contexts.txInfoOutputs info)

        where
            withdrawAda :: Ledger.Value
            withdrawAda = Ada.lovelaceValueOf (withdrawAmount)
-}          


-- | Wrap the minting policy using the boilerplate template haskell code
lcPolicy :: LCMintPolicyParams -> Scripts.MintingPolicy
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






