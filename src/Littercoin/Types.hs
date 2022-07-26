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
)where

import              Data.Aeson                          (FromJSON, ToJSON)  
--import qualified    Data.ByteString.Char8 as B          (ByteString)
import              GHC.Generics                        (Generic)
import qualified    Ledger.Address as Address           (PaymentPubKeyHash(..))
--import qualified    Ledger.Tx as Tx                     (TxOutRef(..))
import qualified    Ledger.Value as Value               (TokenName(..))
import              Playground.Contract as Playground   (ToSchema)
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Bool(..))
import qualified    Prelude as Haskell                  (Show)


-- | The mint policy reeemder indicates if the token is to be minted or burned
data MintPolicyRedeemer = MintPolicyRedeemer
    { mpPolarity                  :: !Bool  -- True = Mint, False = Burn
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''MintPolicyRedeemer [('MintPolicyRedeemer,0)] 
PlutusTx.makeLift ''MintPolicyRedeemer


-- | The mint policy params passes the a utxo, token name and CO2 information as a parameter into the minting poicy which will make the NFT policy unique
data MintPolicyParams = MintPolicyParams
    { 
      mpTokenName                 :: !Value.TokenName
    , mpAdminPkh                  :: !Address.PaymentPubKeyHash     
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)

PlutusTx.makeIsDataIndexed ''MintPolicyParams [('MintPolicyParams,0)] 
PlutusTx.makeLift ''MintPolicyParams

