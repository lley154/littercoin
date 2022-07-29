{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-} 
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Littercoin.Types 
(
     MintPolicyRedeemer(..)
   , LCMintPolicyParams(..)
   , LCRedeemer(..)
   , LCValidatorParams(..)
   , NFTMintPolicyParams(..)     
)where

import              Data.Aeson                          (FromJSON, ToJSON)  
import              GHC.Generics                        (Generic)
import qualified    Ledger.Address as Address           (PaymentPubKeyHash(..))
import qualified    Ledger.Value as Value               (TokenName(..), Value)
import              Playground.Contract as Playground   (ToSchema)
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Bool(..), Integer)
import qualified    Prelude as Haskell                  (Show)


-- | The mint policy reeemder indicates if the token is to be minted or burned
data MintPolicyRedeemer = MintPolicyRedeemer
    { mpPolarity                  :: !Bool  -- True = Mint, False = Burn
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''MintPolicyRedeemer [('MintPolicyRedeemer,0)] 
PlutusTx.makeLift ''MintPolicyRedeemer


-- | The mint policy params passes the token name and AdminPkh as a parameter
--   into the minting poicy which will make the NFT policy unique
data LCMintPolicyParams = LCMintPolicyParams
    { 
      lcTokenName                 :: !Value.TokenName
    , lcAdminPkh                  :: !Address.PaymentPubKeyHash
    , lcNFTTokenValue             :: !Value.Value  
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)

PlutusTx.makeIsDataIndexed ''LCMintPolicyParams [('LCMintPolicyParams,0)] 
PlutusTx.makeLift ''LCMintPolicyParams


-- | The mint policy params passes the token name and adminPkh as a parameter 
--   into the minting poicy which will make the NFT policy unique
data NFTMintPolicyParams = NFTMintPolicyParams
    { 
      nftTokenName                 :: !Value.TokenName
    , nftAdminPkh                  :: !Address.PaymentPubKeyHash 
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)

PlutusTx.makeIsDataIndexed ''NFTMintPolicyParams [('NFTMintPolicyParams,0)] 
PlutusTx.makeLift ''NFTMintPolicyParams

-- | LCValidatorParams is used to pass the admin pkh as a parameter to the 
--   littercoin validator script
data LCValidatorParams = LCValidatorParams
    {   lcvAdminPkh                 :: !Address.PaymentPubKeyHash
    ,   lcvNFTTokenValue            :: !Value.Value
    ,   lcvLCTokenName              :: !Value.TokenName    
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''LCValidatorParams [('LCValidatorParams,0)] 
PlutusTx.makeLift ''LCValidatorParams


-- | The LCRedemeer used to indicate if the action is to mint or burn littercoin or
--   to add and remove Ada from the littercoin contract.   Also specify the amount 
--   as well in the redeemer.
data LCRedeemer = 
       MintLC Integer    -- mint littercoin
     | BurnLC Integer    -- burn littercoin and retreive Ada
     | AddAda Integer     -- add Ada to the smart contract
     
    deriving Haskell.Show


PlutusTx.makeIsDataIndexed
  ''LCRedeemer
  [ ('MintLC, 0),
    ('BurnLC, 1),
    ('AddAda, 2)
  ]
PlutusTx.makeLift ''LCRedeemer

-- | The thread token redeemer passes a utxo from the lotto admin's wallet 
--   to the thread token miting policy which is used to create the lotto 
--   and buy thread tokens
data ThreadTokenRedeemer = ThreadTokenRedeemer
    {   ttTxOutRef :: !Tx.TxOutRef  
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''ThreadTokenRedeemer [('ThreadTokenRedeemer,0)] 
