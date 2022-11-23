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
txIdBS = "570784d01669bcd18196cb6f9b8e8d9393d4162ce54c5e9420efbd6326a47593"

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
ownerTokMPH = "9842d1c0c35c346399c0b18529bd469e30b46c000297c9b932bf151d"


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

{-
-- | Owner Token
otTokName :: Value.TokenName
otTokName = Value.TokenName $ sha2_256 $ sha2_256 txBS
    where
        txBS = (TxV2.getTxId(TxV2.txOutRefId txOutRef')) <> 
                intToBBS(TxV2.txOutRefIdx txOutRef')  


otTokValue :: Value.Value
otTokValue = otVal
  where
    (_, otVal) = Value.split(threadTokenValue threadTokenCurSymbol otTokName)



-}

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



merchantTokenMintParams :: MerchantTokenMintPolicyParams
merchantTokenMintParams = MerchantTokenMintPolicyParams 
    {
        mtTokenName = merchantTokName
    ,   mtAdminPkh = adminPaymentPkh
    ,   mtOwnerTokenValue = ownerTokValue
    }

{-
merchantTokValue :: Value.Value
merchantTokValue = merchantTokVal
  where
    (_, merchantTokVal) = Value.split(merchantTokenValue (merchantTokenCurSymbol merchantTokenMintParams) merchantTokName)

-}

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

    -- Generate datum
    writeDatumInit

    -- Generate redeemers
    writeRedeemerInit
    writeRedeemerAdd
    writeRedeemerMint
    writeRedeemerMintVal
    writeRedeemerMintMerchantToken
    writeRedeemerBurn
    writeRedeemerBurnVal
    writeRedeemerBurnMerchantToken

    -- Generate plutus scripts and hashes
    writeTTMintingPolicy
    writeTTMintingPolicyHash
    writeLCMintingPolicy
    writeLCMintingPolicyHash
    writeMerchantTokenMintingPolicy
    writeMerchantTokenMintingPolicyHash
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
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData AddAda
    in
        LBS.writeFile "deploy/redeemer-add-ada.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)


writeRedeemerMint :: IO ()
writeRedeemerMint = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData MintLC
    in
        LBS.writeFile "deploy/redeemer-mint.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerMintVal :: IO ()
writeRedeemerMintVal = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = True    -- mint token
             ,  mpTotalAdaAmount = 0 -- update with the amount of Ada locked at the littercoin contract
             ,  mpWithdrawAmount = 0 -- ignored during minting   
             }
    in
        LBS.writeFile "deploy/redeemer-mint-val.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)


writeRedeemerMintMerchantToken :: IO ()
writeRedeemerMintMerchantToken = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = True     -- mint token
             ,  mpTotalAdaAmount = 0  -- ingored for MerchantToken minting
             ,  mpWithdrawAmount = 0  -- ignored during minting   
             }
    in
        LBS.writeFile "deploy/redeemer-mint-merchant.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)



writeRedeemerBurn :: IO ()
writeRedeemerBurn = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData BurnLC
    in
        LBS.writeFile "deploy/redeemer-burn.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerBurnVal :: IO ()
writeRedeemerBurnVal = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = False    -- mint token
             ,  mpTotalAdaAmount = 0  -- update with the amount of Ada locked at the littercoin contract
             ,  mpWithdrawAmount = 0  -- upate with amount of Ada to withdrawl from contract   
             }
    in
        LBS.writeFile "deploy/redeemer-burn-val.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerBurnMerchantToken :: IO ()
writeRedeemerBurnMerchantToken = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = False      -- burn token
             ,  mpTotalAdaAmount = 0    -- ingored for MerchantToken burn
             ,  mpWithdrawAmount = 0    -- ingored for MerchantToken burn   
             }
    in
        LBS.writeFile "deploy/redeemer-burn-merchant.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)




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



writeMerchantTokenMintingPolicy :: IO ()
writeMerchantTokenMintingPolicy = void $ writeFileTextEnvelope "deploy/merchant-minting-policy.plutus" Nothing serialisedScript
  where
    script :: PlutusV2.Script
    script = PlutusV2.unMintingPolicyScript $ merchantTokenPolicy merchantTokenMintParams 

    scriptSBS :: SBS.ShortByteString
    scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

    serialisedScript :: PlutusScript PlutusScriptV2
    serialisedScript = PlutusScriptSerialised scriptSBS

writeMerchantTokenMintingPolicyHash :: IO ()
writeMerchantTokenMintingPolicyHash = 
    LBS.writeFile "deploy/merchant-minting-policy.hash" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData mph)
  where
    mph = PlutusTx.toBuiltinData $ PSU.V2.mintingPolicyHash $ merchantTokenPolicy merchantTokenMintParams


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


-- | Decode from hex base 16 to a base 10 bytestring is needed because
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

