{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-} 
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Littercoin.Types 
(
     MintPolicyRedeemer(..)
   , MintPolicyParams(..)   
   , RequiredMetadata(..)
   , FileData(..)
   , OptionalMetadata(..)
)where

import              Data.Aeson                          (FromJSON, ToJSON)  
import qualified    Data.ByteString.Char8 as B          (ByteString)
import              GHC.Generics                        (Generic)
import qualified    Ledger.Address as Address           (PaymentPubKeyHash(..))
import qualified    Ledger.Tx as Tx                     (TxOutRef(..))
import qualified    Ledger.Value as Value               (TokenName(..))
import              Playground.Contract as Playground   (ToSchema)
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Bool(..), BuiltinByteString, Integer)
import qualified    Prelude as Haskell                  (Show)


-- | The mint policy reeemder indicates if the token is to be minted or burned
data MintPolicyRedeemer = MintPolicyRedeemer
    { mpPolarity                  :: !Bool  -- True = Mint, False = Burn
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''MintPolicyRedeemer [('MintPolicyRedeemer,0)] 
PlutusTx.makeLift ''MintPolicyRedeemer


-- | The mint policy params passes the a utxo, token name and CO2 information as a parameter into the minting poicy which will make the NFT policy unique
data MintPolicyParams = MintPolicyParams
    { mpOref                      :: !Tx.TxOutRef
    , mpTokenName                 :: !Value.TokenName
    , mpAddress                   :: !BuiltinByteString
    , mpLat                       :: !Integer
    , mpLong                      :: !Integer
    , mpCategory                  :: !BuiltinByteString
    , mpMethod                    :: !BuiltinByteString
    , mpCO2Qty                    :: !Integer
    , mpAdminPkh                  :: !Address.PaymentPubKeyHash     
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)

PlutusTx.makeIsDataIndexed ''MintPolicyParams [('MintPolicyParams,0)] 
PlutusTx.makeLift ''MintPolicyParams


-- | RequiredMetadata is required because it is used for creating token name which is a hash
--   of they key required values.  This will ensure the token name is unqiue and can also be 
--   validated against the transaction metadata onchain.
data RequiredMetadata = RequiredMetadata
    {
        address         :: !B.ByteString
    ,   lat             :: !Integer
    ,   long            :: !Integer
    ,   category        :: !B.ByteString
    ,   method          :: !B.ByteString
    ,   cO2Qty          :: !Integer
    ,   reg_serial      :: !B.ByteString
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

-- | FileData is optional and only included in the onchain transaction metadata
data FileData = FileData
    {
        file_name       :: B.ByteString
    ,   file_mediaType  :: B.ByteString
    ,   src             :: B.ByteString
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

-- | OptionalMetadat is optional and only included in the onchain transaction metadata
data OptionalMetadata = OptionalMetadata
    {
        name            :: B.ByteString
    ,   image           :: B.ByteString
    ,   mediaType       :: B.ByteString
    ,   description     :: B.ByteString
    ,   files           :: [FileData]
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

