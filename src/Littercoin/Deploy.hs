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
import qualified Data.ByteString.Char8                as B
import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Ledger.Address                       as Address
import           Ledger.Value                         as Value
import           Littercoin.Types
import           Littercoin.OnChain
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PTSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as Contexts
import qualified Plutus.V2.Ledger.Tx                  as TxV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, FilePath, Semigroup (..),
                                                       Show (..), print, (.),
                                                       String)




-------------------------------------------------------------------------------------
-- START - Littercoin Token Metadata - A value entry has a max 64 UTF-8 Byte Limit
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-- Please note that changes the following token metadata requires this 
-- file to be compiled again and deployed with the updated values.
-------------------------------------------------------------------------------------
{-

-- **** REQUIRED METADATA FIELDS ***

requiredMetadata :: RequiredMetadata
requiredMetadata = RequiredMetadata
    {
        address = "123 Street"
    ,   lat =  4560436487
    ,   long = -7737826809
    ,   category = "Efficiency"
    ,   method = "Solar"                        
    ,   cO2Qty = 50                             
    ,   reg_serial = "123-456-789-10101010"     
    }


-- **** OPTIONAL METADATA FIELDS ***

optionalMetadata :: OptionalMetadata
optionalMetadata = OptionalMetadata
    {
        name = "Solar Pannels"
    ,   image = "ipfs/QmT3rYtkkw4wFBP5SfxENAfDY9NuYoZAz2HVng4cQqnVZe"
    ,   mediaType = "image/png"
    ,   description = "Carbon credit offset in metric tons"
    ,   files = [fileData]
    }


fileData :: FileData
fileData = FileData
    {
         file_name = "Carbon Credit"
    ,    file_mediaType = "image/png"
    ,    src = "ipfs://QmT3rYtkkw4wFBP5SfxENAfDY9NuYoZAz2HVng4cQqnVZe" 
    }

-}

-------------------------------------------------------------------------------------
-- END - Littercoin Metadata
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
-- START - Littercoin Minting Policy Parameters 
-------------------------------------------------------------------------------------
-- These are dummy values and need to be replaced with real values for
-- the appropriate enviornment (eg devnet, testnet or mainnet)
-------------------------------------------------------------------------------------

-- Admin spending UTXO
txIdBS :: B.ByteString
txIdBS = "52ff8bc4279a68dced5b1b6b86953f080bf162dd479d52b5cc5952e56d8e4fc4"

-- Admin spending UTXO index
txIdIdxInt :: Integer
txIdIdxInt = 1

-- Admin public key payment hash
adminPubKeyHashBS :: B.ByteString
adminPubKeyHashBS = "25dbf2cdb12487dbe244d48d236ccbd7eefc1a0320c9df638e153df9"

lcTokName :: PlutusV2.TokenName
lcTokName = "Littercoin"

nftTokName :: PlutusV2.TokenName
nftTokName = "Littercoin Approved Merchant"

lcDatum :: LCDatum
lcDatum = LCDatum 
    {   adaAmount = 0                                         
    ,   lcAmount = 0
    }

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

mintRed = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
    {
       mpPolarity = True  -- mint token
    ,  mpWithdrawAmount = 0 -- ignored during minting   
    }

mintParams = LCMintPolicyParams 
    {
        lcTokenName = lcTokName -- the name of the littercoin
    ,   lcAdminPkh = adminPaymentPkh  -- the admin pkh who can only mint littercoins
    ,   lcNFTTokenValue = nftTokValue  -- this contains the NFT that merchants used for burning
    }


lcvParams :: LCValidatorParams
lcvParams = LCValidatorParams
    {   lcvAdminPkh         = adminPaymentPkh
    ,   lcvNFTTokenValue    = nftTokValue
    ,   lcvLCTokenName      = lcTokName
    ,   lcvThreadTokenValue = ttTokValue
    }


{-

mph :: MintingPolicyHash
mph = Scripts.mintingPolicyHash $ policy mintParams


-- Following the Cardano NFT metadatata standard https://cips.cardano.org/cips/cip25/
tokenMetadata :: Data.Aeson.Value
tokenMetadata = object
    [   "721" .= object
        [decodeUtf8 (B.pack (show mph)) .= object
                    [decodeUtf8(B.pack (replace "0x" "" (show tokenName))) .= object 
                            [   "name" .= (decodeUtf8 $ name optionalMetadata),
                                "image" .= (decodeUtf8 $ image optionalMetadata),
                                "mediaType" .= (decodeUtf8 $ mediaType optionalMetadata),
                                "description" .= (decodeUtf8 $ description optionalMetadata),
                                "files" .= [object
                                    [
                                        "name" .= (decodeUtf8 $ file_name ((files optionalMetadata)!!0)),
                                        "mediaType" .= (decodeUtf8 $ file_mediaType ((files optionalMetadata)!!0)),
                                        "src" .= (decodeUtf8 $ src ((files optionalMetadata)!!0))
                                    ] ],
                                "required" .= object
                                    [
                                        "address" .= (decodeUtf8 $ address requiredMetadata),
                                        "lat" .= (lat requiredMetadata),
                                        "long" .= (long requiredMetadata),
                                        "category" .= (decodeUtf8 $ category requiredMetadata),
                                        "method" .= (decodeUtf8 $ method requiredMetadata),
                                        "cO2Qty" .= (cO2Qty requiredMetadata),
                                        "registrarSerialNo" .= (decodeUtf8 $ reg_serial requiredMetadata)
                                    ]
                            ]
                    ]
        , "version" .= ("1.0" :: Haskell.String)
        ]
    ]

-}
-------------------------------------------------------------------------------------
-- END - Derived values 
-------------------------------------------------------------------------------------
 

main::IO ()
main = do



    -- Generate token name and metadata
    writeTTTokenName
    writeLCTokenName
    --writeLCTokenMetadata
    writeNFTTokenName
    --writeNFTTokenMetadata
    
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
                mpPolarity = True  -- mint token
             ,  mpWithdrawAmount = 0 -- ignored during minting   
             }
    in
        LBS.writeFile "deploy/redeemer-mint-lc.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)


writeRedeemerMintNFT :: IO ()
writeRedeemerMintNFT = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = True  -- mint token
             ,  mpWithdrawAmount = 0 -- ignored during minting   
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
                mpPolarity = False  -- mint token
             ,  mpWithdrawAmount = 0 -- upate with amount of Ada to withdrawl from contract   
             }
    in
        LBS.writeFile "deploy/redeemer-burn-lc.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)

writeRedeemerBurnNFT :: IO ()
writeRedeemerBurnNFT = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = False  -- mint token
             ,  mpWithdrawAmount = 0 -- upate with amount of Ada to withdrawl from contract   
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
decodeHex :: B.ByteString -> P.BuiltinByteString
decodeHex hexBS =    
         case getTx of
            Right decHex -> do
                --putStrLn $ "Tx name: " ++ show t
                P.toBuiltin(decHex)  
            Left _ -> do
                --putStrLn $ "No Token name: " ++ show e
                P.emptyByteString 
                
        where        
            getTx :: Either String B.ByteString = B16.decode hexBS

