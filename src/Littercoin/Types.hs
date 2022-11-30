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
   , ThreadTokenRedeemer(..)    

)where

import              Data.Aeson                          (FromJSON, ToJSON)  
import              GHC.Generics                        (Generic)
import qualified    Ledger.Address as Address           (PaymentPubKeyHash(..))
import qualified    Ledger.Value as Value               (CurrencySymbol(..), TokenName(..), Value)
import qualified    Plutus.V2.Ledger.Tx as Tx           (TxOutRef(..))
import              Playground.Contract as Playground   (ToSchema)
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Bool(..), Integer)
import qualified    Prelude as Haskell                  (Show)


-- | The thread token redeemer passes a utxo from the Littercoin admin's wallet 
--   to the thread token miting policy which is used to create the a thread token.
--   A Thread token is needed to make sure we always include the correct validator
--   script as an input to a new transaction.
data ThreadTokenRedeemer = ThreadTokenRedeemer
    {   ttTxOutRef :: Tx.TxOutRef  
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''ThreadTokenRedeemer [('ThreadTokenRedeemer,0)] 


-- | The mint policy reeemder indicates if the token is to be minted or burned
data MintPolicyRedeemer = MintPolicyRedeemer
    { 
      mpPolarity                  :: Bool     -- True = Mint, False = Burn
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''MintPolicyRedeemer [('MintPolicyRedeemer,0)] 
PlutusTx.makeLift ''MintPolicyRedeemer


-- | The Littercoin mintint policy params passes token name, reserve amount and most
--   importantly the utxo that will be spent to ensure this is a unique minting policy
data LCMintPolicyParams = LCMintPolicyParams
    { 
      lcTokenName                   :: Value.TokenName
    , lcReserveAmt                  :: Integer
    , lcTxOutRef                    :: Tx.TxOutRef  
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)

PlutusTx.makeIsDataIndexed ''LCMintPolicyParams [('LCMintPolicyParams,0)] 
PlutusTx.makeLift ''LCMintPolicyParams


-- | LCValidatorParams is used to pass the admin pkh, MerchantToken & Littercoin token names as a parameter to the 
--   littercoin validator script
data LCValidatorParams = LCValidatorParams
    {   lcvAdminPkh                 :: Address.PaymentPubKeyHash
    ,   lcvLCTokenName              :: Value.TokenName 
    ,   lcvLCTokenCurSymbol         :: Value.CurrencySymbol 
    ,   lcvMerchantTokenValue       :: Value.Value     
    ,   lcvThreadTokenValue         :: Value.Value      
    ,   lcvOwnerTokenValue          :: Value.Value      
    ,   lcvDonationTokenValue       :: Value.Value   
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''LCValidatorParams [('LCValidatorParams,0)] 
PlutusTx.makeLift ''LCValidatorParams


-- | The LCRedemeer used to indicate if the action is to mint or burn littercoin or
--   to add and remove Ada from the littercoin contract.   Also specify the amount 
--   as well in the redeemer.
data LCRedeemer = 
       MintLC Integer    -- mint littercoin & sequence number
     | BurnLC Integer    -- burn littercoin and retreive Ada & sequence number
     | AddAda Integer    -- add Ada to the smart contract & sequence number
     | SpendAction       -- spend the action utxo at the script address
     
    deriving Haskell.Show

PlutusTx.makeIsDataIndexed
  ''LCRedeemer
  [ ('MintLC, 0),
    ('BurnLC, 1),
    ('AddAda, 2),
    ('SpendAction, 3)
  ]
PlutusTx.makeLift ''LCRedeemer


