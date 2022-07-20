{-# LANGUAGE DataKinds         		#-}
{-# LANGUAGE NoImplicitPrelude 		#-}
{-# LANGUAGE TemplateHaskell 		#-}
{-# LANGUAGE ScopedTypeVariables	#-}
{-# LANGUAGE TypeApplications    	#-}
{-# LANGUAGE ImportQualifiedPost	#-}

module Littercoin.OnChain 
  ( serialisedScript,
    scriptSBS,
    writeSerialisedScript,
  )
where



import Cardano.Api (PlutusScriptV2, writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (..), ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), fromPlutusData, scriptDataToJson)
import Codec.Serialise
import Data.Functor (void)
import PlutusTx qualified
import PlutusTx.Prelude qualified
import Plutus.V2.Ledger.Api qualified as PlutusV2
--import Plutus.Script.Utils.V2.Scripts as Utils
import Plutus.Script.Utils.V2.Typed.Scripts.Validators as Utils
import Ledger.Address as Addr
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Prelude (IO, Semigroup (..), print, (.), ($), Maybe(Nothing))



newtype MyCustomDatum = MyCustomDatum PlutusTx.Prelude.Integer
PlutusTx.unstableMakeIsData ''MyCustomDatum
newtype MyCustomRedeemer = MyCustomRedeemer PlutusTx.Prelude.Integer
PlutusTx.unstableMakeIsData ''MyCustomRedeemer

-- This validator always validates true
{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatum -> MyCustomRedeemer -> PlutusV2.ScriptContext -> PlutusTx.Prelude.Bool
mkValidator _ _ _ = PlutusTx.Prelude.True

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator


{-
   As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise $ validator

{-
   As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "OnChain.plutus" Nothing serialisedScript


