{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

module Littercoin.Deploy
    ( main
    ) where


import           Cardano.Api                          (PlutusScript,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise                      (serialise)
import           Data.Aeson                           (encode)
import qualified Data.ByteString.Char8                as B (ByteString)
import qualified Data.ByteString.Base16               as B16 (decode)
import qualified Data.ByteString.Lazy                 as LBS (toStrict, writeFile)
import qualified Data.ByteString.Short                as SBS(ShortByteString, toShort)
import           Data.Functor                         (void)
import qualified Ledger.Address                       as Address
import           Ledger.Value                         as Value
import           Littercoin.Types
import           Littercoin.OnChain
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PTSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Tx                  as TxV2
import qualified PlutusTx                             (toBuiltinData)
import           PlutusTx.Prelude                     (BuiltinByteString, BuiltinData, Bool(..), Either(..), 
                                                       emptyByteString , Integer, Maybe(..), return, sha2_256, 
                                                       toBuiltin, ($))
import           Prelude                              (IO, Semigroup (..), String, (.))



-------------------------------------------------------------------------------------
-- START - Littercoin Minting Policy Parameters 
-------------------------------------------------------------------------------------
-- These are dummy values and need to be replaced with real values for
-- the appropriate enviornment (eg devnet, testnet or mainnet)
-------------------------------------------------------------------------------------

-- Admin spending UTXO
txIdBS :: B.ByteString
txIdBS = "1e9246392aaa26defadd56ec143483ea4849f8e9ece809853dd9f5dc080faad3"

-- Admin spending UTXO index
txIdIdxInt :: Integer
txIdIdxInt = 0

-- Admin public key payment hash
adminPubKeyHashBS :: B.ByteString
adminPubKeyHashBS = "b9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682"
                     
lcTokName :: PlutusV2.TokenName
lcTokName = "Littercoin"

merchantTokName :: PlutusV2.TokenName
merchantTokName = "Merchant Token Littercoin"

merchantTokMPH :: PlutusV2.MintingPolicyHash
merchantTokMPH = "66e10f73639b97d976275fe11778fadab379c1bf7cedd4dfb4219d9b"

ownerTokName :: PlutusV2.TokenName
ownerTokName = "Owner Token Littercoin"

ownerTokMPH :: PlutusV2.MintingPolicyHash
ownerTokMPH = "66e10f73639b97d976275fe11778fadab379c1bf7cedd4dfb4219d9b"

donationTokName :: PlutusV2.TokenName
donationTokName = "Donation Littercoin"

donationTokMPH :: PlutusV2.MintingPolicyHash
donationTokMPH = "d17c72f434ff89d37a1b1ee88721ae5aa0809eaafd1ac3d1df248794"


-------------------------------------------------------------------------------------
-- END - Littercoin Minting Policy Parameters 
-------------------------------------------------------------------------------------

   
-------------------------------------------------------------------------------------
-- START - Derived values
-------------------------------------------------------------------------------------

adminPaymentPkh :: Address.PaymentPubKeyHash
adminPaymentPkh = Address.PaymentPubKeyHash (PlutusV2.PubKeyHash $ decodeHex adminPubKeyHashBS)


txOutRef' :: TxV2.TxOutRef
txOutRef' = TxV2.TxOutRef
        {
            TxV2.txOutRefId = TxV2.TxId
            {
                TxV2.getTxId = decodeHex txIdBS
            } 
        ,   TxV2.txOutRefIdx = txIdIdxInt
        }

-- | Thread Token
ttTokName :: Value.TokenName
ttTokName = Value.TokenName $ sha2_256 txBS
    where
        txBS = (TxV2.getTxId(TxV2.txOutRefId txOutRef')) <> 
                intToBBS(TxV2.txOutRefIdx txOutRef')  


ttTokValue :: Value.Value
ttTokValue = ttVal
  where
    (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol ttTokName)


ownerTokValue :: Value.Value
ownerTokValue = Value.singleton ownerTokCurSymbol ownerTokName 1
  where
    ownerTokCurSymbol :: CurrencySymbol
    ownerTokCurSymbol = Value.mpsSymbol ownerTokMPH


merchantTokValue :: Value.Value
merchantTokValue = Value.singleton merchantTokCurSymbol merchantTokName 1
  where
    merchantTokCurSymbol :: CurrencySymbol
    merchantTokCurSymbol = Value.mpsSymbol merchantTokMPH

donationTokValue :: Value.Value
donationTokValue = Value.singleton donationTokCurSymbol donationTokName 1
  where
    donationTokCurSymbol :: CurrencySymbol
    donationTokCurSymbol = Value.mpsSymbol donationTokMPH




mintParams :: LCMintPolicyParams
mintParams = LCMintPolicyParams 
    {
        lcTokenName = lcTokName -- the name of the littercoin
    ,   lcAdminPkh = adminPaymentPkh  -- the admin pkh who can only mint littercoins
    ,   lcThreadTokenValue = ttTokValue
    ,   lcMerchantTokenValue = merchantTokValue  -- this contains the MerchantToken that merchants used for burning
    ,   lcOwnerTokenValue = ownerTokValue
    }

lcvParams :: LCValidatorParams
lcvParams = LCValidatorParams
    {   lcvTokenName = lcTokName
    ,   lcvAdminPkh = adminPaymentPkh
    ,   lcvMerchantTokenValue = merchantTokValue
    ,   lcvThreadTokenValue = ttTokValue
    ,   lcvOwnerTokenValue = ownerTokValue
    ,   lcvDonationTokenValue = donationTokValue
    }


-------------------------------------------------------------------------------------
-- END - Derived values 
-------------------------------------------------------------------------------------
 

main::IO ()
main = do

    -- Generate token name and metadata
    writeTTTokenName
    writeTTTokenValue    
    writeOwnerTokenName
    writeOwnerTokenValue    
    writeLCTokenName
    writeMerchantTokenName
    writeMerchantTokenValue
    writeDonationTokenName
    writeDonationTokenValue

    -- Generate datum
    writeDatumInit

    -- Generate redeemers
    writeRedeemerInit
    writeRedeemerAdd
    writeRedeemerMint
    writeRedeemerMintVal
    writeRedeemerBurn
    writeRedeemerBurnVal
    writeRedeemerSpendAction
    writeRedeemerSpendAction

    -- Generate plutus scripts and hashes
    writeTTMintingPolicy
    writeTTMintingPolicyHash
    writeLCMintingPolicy
    writeLCMintingPolicyHash
    writeLCValidator
    writeLCValidatorHash
    
    return ()


writeTTTokenValue :: IO ()
writeTTTokenValue = 
    LBS.writeFile "deploy/thread-token-value.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData ttTokValue)    


writeTTTokenName :: IO ()
writeTTTokenName = 
    LBS.writeFile "deploy/thread-token-name.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData ttTokName)    


writeOwnerTokenValue :: IO ()
writeOwnerTokenValue = 
    LBS.writeFile "deploy/owner-token-value.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData ownerTokValue)    


writeOwnerTokenName :: IO ()
writeOwnerTokenName = 
    LBS.writeFile "deploy/owner-token-name.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData ownerTokName)    


writeLCTokenName :: IO ()
writeLCTokenName = 
    LBS.writeFile "deploy/lc-token-name.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData lcTokName)    

writeMerchantTokenName :: IO ()
writeMerchantTokenName = 
    LBS.writeFile "deploy/merchant-token-name.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData merchantTokName)    

writeMerchantTokenValue :: IO ()
writeMerchantTokenValue = 
    LBS.writeFile "deploy/merchant-token-value.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData merchantTokValue)    


writeDonationTokenName :: IO ()
writeDonationTokenName = 
    LBS.writeFile "deploy/donation-token-name.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData donationTokName)    

writeDonationTokenValue :: IO ()
writeDonationTokenValue = 
    LBS.writeFile "deploy/donation-token-value.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData donationTokValue)    



writeDatumInit :: IO ()
writeDatumInit = 
    let lcDatum = LCDatum 
            {   lcAdaAmount = 0                                         
            ,   lcAmount = 0
            }
        dat = PlutusTx.toBuiltinData lcDatum
    in
        LBS.writeFile "deploy/lc-datum-init.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData dat)



writeRedeemerInit :: IO ()
writeRedeemerInit = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ ThreadTokenRedeemer txOutRef'
    in
        LBS.writeFile "deploy/redeemer-thread-token-mint.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)


writeRedeemerAdd :: IO ()
writeRedeemerAdd = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ AddAda 123
    in
        LBS.writeFile "deploy/redeemer-add-ada.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)


writeRedeemerMintVal :: IO ()
writeRedeemerMintVal = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintLC 123
    in
        LBS.writeFile "deploy/redeemer-mint-val.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerMint :: IO ()
writeRedeemerMint = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = True    -- mint token
             ,  mpTotalAdaAmount = 0 -- update with the amount of Ada locked at the littercoin contract
             ,  mpWithdrawAmount = 0 -- ignored during minting   
             }
    in
        LBS.writeFile "deploy/redeemer-mint.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)



writeRedeemerBurnVal :: IO ()
writeRedeemerBurnVal = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ BurnLC 123
    in
        LBS.writeFile "deploy/redeemer-burn-val.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerBurn :: IO ()
writeRedeemerBurn = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = False    -- mint token
             ,  mpTotalAdaAmount = 0  -- update with the amount of Ada locked at the littercoin contract
             ,  mpWithdrawAmount = 0  -- upate with amount of Ada to withdrawl from contract   
             }
    in
        LBS.writeFile "deploy/redeemer-burn.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)



writeRedeemerSpendAction :: IO ()
writeRedeemerSpendAction = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData SpendAction
    in
        LBS.writeFile "deploy/redeemer-spend-action.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)



writeTTMintingPolicy :: IO ()
writeTTMintingPolicy = void $ writeFileTextEnvelope "deploy/thread-token-minting-policy.plutus" Nothing serialisedScript
  where
    script :: PlutusV2.Script
    script = PlutusV2.unMintingPolicyScript threadTokenPolicy 

    scriptSBS :: SBS.ShortByteString
    scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

    serialisedScript :: PlutusScript PlutusScriptV2
    serialisedScript = PlutusScriptSerialised scriptSBS


writeTTMintingPolicyHash :: IO ()
writeTTMintingPolicyHash = 
    LBS.writeFile "deploy/thread-token-minting-policy.hash" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData mph)
  where
    mph = PlutusTx.toBuiltinData $ PSU.V2.mintingPolicyHash threadTokenPolicy


writeLCMintingPolicy :: IO ()
writeLCMintingPolicy = void $ writeFileTextEnvelope "deploy/lc-minting-policy.plutus" Nothing serialisedScript
  where
    script :: PlutusV2.Script
    script = PlutusV2.unMintingPolicyScript $ lcPolicy mintParams 

    scriptSBS :: SBS.ShortByteString
    scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

    serialisedScript :: PlutusScript PlutusScriptV2
    serialisedScript = PlutusScriptSerialised scriptSBS

writeLCMintingPolicyHash :: IO ()
writeLCMintingPolicyHash = 
    LBS.writeFile "deploy/lc-minting-policy.hash" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData mph)
  where
    mph = PlutusTx.toBuiltinData $ PSU.V2.mintingPolicyHash $ lcPolicy mintParams


writeLCValidator :: IO ()
writeLCValidator = void $ writeFileTextEnvelope "deploy/lc-validator.plutus" Nothing serialisedScript
  where
    script :: BuiltinData -> PSU.V2.Validator
    script = lcValidator

    scriptSBS :: SBS.ShortByteString
    scriptSBS = SBS.toShort . LBS.toStrict $ serialise $ script $ PlutusTx.toBuiltinData lcvParams

    serialisedScript :: PlutusScript PlutusScriptV2
    serialisedScript = PlutusScriptSerialised scriptSBS

writeLCValidatorHash :: IO ()
writeLCValidatorHash = 
    LBS.writeFile "deploy/lc-validator.hash" $ encode $ PlutusTx.toBuiltinData $ PTSU.V2.validatorHash $ typedLCValidator $ PlutusTx.toBuiltinData lcvParams


-- | Decode from hex base 16 to a base 10 bytestring because
--   that is how it is stored in the ledger onchain
decodeHex :: B.ByteString -> BuiltinByteString
decodeHex hexBS =    
         case getTx of
            Right decHex -> do
                --putStrLn $ "Tx name: " ++ show t
                toBuiltin(decHex)  
            Left _ -> do
                --putStrLn $ "No Token name: " ++ show e
                emptyByteString 
                
        where        
            getTx :: Either String B.ByteString = B16.decode hexBS

