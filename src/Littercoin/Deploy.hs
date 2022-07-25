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
  ( --policy
  )
where

import           Cardano.Api                    (PlutusScriptV2,
                                                 writeFileTextEnvelope)
import           Cardano.Api.Shelley            (PlutusScript (PlutusScriptSerialised))
import           Codec.Serialise
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import           Data.Functor                   (void)
import           Ledger
import qualified Ledger.Typed.Scripts           as Scripts
import           Ledger.Value                   as Value
import           Littercoin.OnChain             (policy)
import           Littercoin.Types               (MintPolicyRedeemer(..), MintPolicyParams(..))
import           GHC.Generics                   (Generic)
import           Playground.Contract as Playground   (ToSchema)
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
import           Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as MP
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V2.Ledger.Contexts      as PlutusV2
import           PlutusTx                       (CompiledCode)
import qualified PlutusTx
import           PlutusTx.Prelude               as P hiding (Semigroup (..),
                                                      unless, (.))
import           Prelude as Haskell             (IO, Show, (.))



{-
    As a Script
-}

script :: PlutusV2.Script
script = PlutusV2.unMintingPolicyScript $ policy mintPolicyRedeemer


{-
    As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

{-
    As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "nft-mint-V2.plutus" Nothing serialisedScript
