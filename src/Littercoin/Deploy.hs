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
txIdBS = "31271ea0d11e161871d6e573ad22a3da7672a62018879333132d6d5b89d633aa"

-- Admin spending UTXO index
txIdIdxInt :: Integer
txIdIdxInt = 1

-- Admin public key payment hash
adminPubKeyHashBS :: B.ByteString
adminPubKeyHashBS = "21b445483755338e2b38b5c47bd2873f887e0cebd7e9507384f556bc"

lcTokName :: PlutusV2.TokenName
lcTokName = "Littercoin"

nftTokName :: PlutusV2.TokenName
nftTokName = "Littercoin Approved Merchant"


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


ttTokName :: Value.TokenName
ttTokName = Value.TokenName $ sha2_256 txBS
    where
        txBS = (TxV2.getTxId(TxV2.txOutRefId txOutRef')) <> 
                intToBBS(TxV2.txOutRefIdx txOutRef')  


ttTokValue :: Value.Value
ttTokValue = ttVal
  where
    (_, ttVal) = Value.split(threadTokenValue threadTokenCurSymbol ttTokName)



nftMintParams :: NFTMintPolicyParams
nftMintParams = NFTMintPolicyParams 
    {
        nftTokenName = nftTokName
    ,   nftAdminPkh = adminPaymentPkh
    }

nftTokValue :: Value.Value
nftTokValue = nftTokVal
  where
    (_, nftTokVal) = Value.split(nftTokenValue (nftCurSymbol nftMintParams) nftTokName)


mintParams :: LCMintPolicyParams
mintParams = LCMintPolicyParams 
    {
        lcTokenName = lcTokName -- the name of the littercoin
    ,   lcAdminPkh = adminPaymentPkh  -- the admin pkh who can only mint littercoins
    ,   lcThreadTokenValue = ttTokValue
    ,   lcNFTTokenValue = nftTokValue  -- this contains the NFT that merchants used for burning
    }

lcvParams :: LCValidatorParams
lcvParams = LCValidatorParams
    {   lcvAdminPkh         = adminPaymentPkh
    ,   lcvNFTTokenValue    = nftTokValue
    ,   lcvLCTokenName      = lcTokName
    ,   lcvThreadTokenValue = ttTokValue
    }


-------------------------------------------------------------------------------------
-- END - Derived values 
-------------------------------------------------------------------------------------
 

main::IO ()
main = do

    -- Generate token name and metadata
    writeTTTokenName
    writeLCTokenName
    writeNFTTokenName

    -- Generate datum
    writeDatumInit

    -- Generate redeemers
    writeRedeemerInit
    writeRedeemerAdd
    writeRedeemerMint
    writeRedeemerMintLC
    writeRedeemerMintNFT
    writeRedeemerBurn
    writeRedeemerBurnLC
    writeRedeemerBurnNFT

    -- Generate plutus scripts and hashes
    writeTTMintingPolicy
    writeTTMintingPolicyHash
    writeLCMintingPolicy
    writeLCMintingPolicyHash
    writeNFTMintingPolicy
    writeNFTMintingPolicyHash
    writeLCValidator
    writeLCValidatorHash
    
    return ()

writeTTTokenName :: IO ()
writeTTTokenName = 
    LBS.writeFile "deploy/thread-token-name.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData ttTokName)    

writeLCTokenName :: IO ()
writeLCTokenName = 
    LBS.writeFile "deploy/lc-token-name.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData lcTokName)    

writeNFTTokenName :: IO ()
writeNFTTokenName = 
    LBS.writeFile "deploy/nft-token-name.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData nftTokName)    


writeDatumInit :: IO ()
writeDatumInit = 
    let lcDatum = LCDatum 
            {   adaAmount = 0                                         
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
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ AddAda 42
    in
        LBS.writeFile "deploy/redeemer-add-ada.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)


writeRedeemerMint :: IO ()
writeRedeemerMint = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintLC 42
    in
        LBS.writeFile "deploy/redeemer-mint.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerMintLC :: IO ()
writeRedeemerMintLC = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = True    -- mint token
             ,  mpTotalAdaAmount = 0 -- update with the amount of Ada locked at the littercoin contract
             ,  mpWithdrawAmount = 0 -- ignored during minting   
             }
    in
        LBS.writeFile "deploy/redeemer-mint-lc.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)


writeRedeemerMintNFT :: IO ()
writeRedeemerMintNFT = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = True     -- mint token
             ,  mpTotalAdaAmount = 0  -- ingored for NFT minting
             ,  mpWithdrawAmount = 0  -- ignored during minting   
             }
    in
        LBS.writeFile "deploy/redeemer-mint-nft.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)



writeRedeemerBurn :: IO ()
writeRedeemerBurn = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ BurnLC 42
    in
        LBS.writeFile "deploy/redeemer-burn.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerBurnLC :: IO ()
writeRedeemerBurnLC = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = False    -- mint token
             ,  mpTotalAdaAmount = 0  -- update with the amount of Ada locked at the littercoin contract
             ,  mpWithdrawAmount = 0  -- upate with amount of Ada to withdrawl from contract   
             }
    in
        LBS.writeFile "deploy/redeemer-burn-lc.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerBurnNFT :: IO ()
writeRedeemerBurnNFT = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = False      -- burn token
             ,  mpTotalAdaAmount = 0    -- ingored for NFT burn
             ,  mpWithdrawAmount = 0    -- ingored for NFT burn   
             }
    in
        LBS.writeFile "deploy/redeemer-burn-nft.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)




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



writeNFTMintingPolicy :: IO ()
writeNFTMintingPolicy = void $ writeFileTextEnvelope "deploy/nft-minting-policy.plutus" Nothing serialisedScript
  where
    script :: PlutusV2.Script
    script = PlutusV2.unMintingPolicyScript $ nftPolicy nftMintParams 

    scriptSBS :: SBS.ShortByteString
    scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

    serialisedScript :: PlutusScript PlutusScriptV2
    serialisedScript = PlutusScriptSerialised scriptSBS

writeNFTMintingPolicyHash :: IO ()
writeNFTMintingPolicyHash = 
    LBS.writeFile "deploy/nft-minting-policy.hash" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData mph)
  where
    mph = PlutusTx.toBuiltinData $ PSU.V2.mintingPolicyHash $ nftPolicy nftMintParams


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

