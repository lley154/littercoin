{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}

module PabContract(
    Contracts(..)
    ) where

import           Littercoin.Littercoin
import           Data.Aeson                          (FromJSON (..), ToJSON (..))                                                      
import qualified Data.OpenApi                        as OpenApi
import           GHC.Generics                        (Generic)
import           Prettyprinter                       (Pretty (..), viaShow)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Prelude                             hiding (init)


-- | Setup contracts that are used by the PAB
data Contracts =  InitContract
                | UseContract
                      deriving (Eq, Ord, Show, Generic)
                      deriving anyclass OpenApi.ToSchema
                      deriving anyclass (FromJSON, ToJSON)

instance Pretty Contracts where
    pretty = viaShow
 
-- | Map PAB Contracts to endpoints
instance Builtin.HasDefinitions Contracts where
    getDefinitions = [ InitContract, UseContract ]
    getSchema =  \case
        InitContract    -> Builtin.endpointsToSchemas @InitSchema 
        UseContract     -> Builtin.endpointsToSchemas @TokenSchema   
   
    getContract = \case
        InitContract    -> Builtin.SomeBuiltin initEndpoint
        UseContract     -> Builtin.SomeBuiltin useEndpoint