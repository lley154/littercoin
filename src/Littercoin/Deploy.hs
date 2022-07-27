{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

module Littercoin.Deploy
    ( main
    ) where

import           Littercoin.OnChain                (policy)
import           Littercoin.Types                  (MintPolicyRedeemer(..), MintPolicyParams(..))
import           Littercoin.Utils                  (decodeHex, integerToBS)
import           Cardano.Api.Shelley                (FileError, PlutusScript (..), PlutusScriptV1, ScriptData(..), scriptDataToJson, 
                                                     ScriptDataJsonSchema(..), writeFileTextEnvelope)
import           Codec.Serialise                    (serialise)
import           Data.Aeson                         (encode, object, Value, (.=))          
import qualified Data.ByteString.Char8 as B         (pack, ByteString)
import qualified Data.ByteString.Lazy as LBS        (toStrict, writeFile)
import qualified Data.ByteString.Short as SBS       (toShort)
import           Data.List.Utils                    (replace)
import           Data.Text.Encoding                 (decodeUtf8)
import qualified Ledger                             (getTxId, MintingPolicy, PaymentPubKeyHash(..), PubKeyHash(..), Validator(..), TxId(..), 
                                                     TxOutRef(..), TxOutRef(txOutRefIdx), unValidatorScript )                          
import           Ledger.Scripts as Scripts          (mintingPolicyHash, MintingPolicyHash, Redeemer(..), unMintingPolicyScript)
import qualified Ledger.Value as Value              (TokenName(..))
import           PlutusTx                           (Data (..), toBuiltinData, ToData, toData)
import           PlutusTx.Prelude                   (Bool(..), Either(..), Integer, Maybe(..), return, sha2_256, ($), (<$>), (<>), (.), (!!))
import           Prelude as Haskell                 (FilePath, IO, show, String)





-------------------------------------------------------------------------------------
-- START - Littercoin Token Metadata - A value entry has a max 64 UTF-8 Byte Limit
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-- Please note that changes the following token metadata requires this 
-- file to be compiled again and deployed with the updated values.
-------------------------------------------------------------------------------------

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
txIdBS = "eb60c323585934b66e5dbbf7a1fac4652c17902e4bc0e8768f70cd7ca8c4fd63"

-- Admin spending UTXO index
txIdIdxInt :: Integer
txIdIdxInt = 0

-- Admin public key payment hash
adminPubKeyHashBS :: B.ByteString
adminPubKeyHashBS = "a766096168c31739f1b52ee287d5b27ad0f68ba76462301565406419"

-------------------------------------------------------------------------------------
-- END - Littercoin Minting Policy Parameters 
-------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------
-- START - Derived values
-------------------------------------------------------------------------------------

tokenName :: Value.TokenName
tokenName = Value.TokenName $ sha2_256 tn
    where
        tn = (decodeHex $ address requiredMetadata) <> 
             (integerToBS $ lat requiredMetadata) <> 
             (integerToBS $ long requiredMetadata) <> 
             (decodeHex $ category requiredMetadata) <> 
             (decodeHex $ method requiredMetadata) <> 
             (integerToBS $ cO2Qty requiredMetadata)


adminPaymentPkh :: Ledger.PaymentPubKeyHash
adminPaymentPkh = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex adminPubKeyHashBS)


txOutRef' :: Ledger.TxOutRef
txOutRef' = Ledger.TxOutRef
        {
            Ledger.txOutRefId = Ledger.TxId
            {
                Ledger.getTxId = decodeHex txIdBS
            } 
        ,   Ledger.txOutRefIdx = txIdIdxInt
        }

mintParams :: MintPolicyParams
mintParams = MintPolicyParams 
                    {
                      mpOref = txOutRef'
                    , mpTokenName = tokenName
                    , mpAddress = decodeHex $ address requiredMetadata
                    , mpLat = lat requiredMetadata
                    , mpLong = long requiredMetadata
                    , mpCategory = decodeHex $ category requiredMetadata
                    , mpMethod = decodeHex $ method requiredMetadata
                    , mpCO2Qty = cO2Qty requiredMetadata
                    , mpAdminPkh = adminPaymentPkh
                    }


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

-------------------------------------------------------------------------------------
-- END - Derived values 
-------------------------------------------------------------------------------------

main::IO ()
main = do

    -- Generate plutus scripts and hashes
    _ <- writeMintingPolicy
    writeMintingPolicyHash

    -- Generate token name and metadata
    writeTokenName
    writeTokenMetadata
    
    -- Generate redeemers
    writeRedeemerMint
    writeRedeemerBurn
    
    return ()
 
-- | Conversion functions from Plutus Builtin datatypes to plutus script data types that are 
--   run on the cardano blockchain 
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x', dataToScriptData y) | (x', y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs


writeJSON :: ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData


writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript 


writeRedeemerMint :: IO ()
writeRedeemerMint = 

    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     }
    in
        writeJSON "deploy/redeemer-token-mint.json" red


writeRedeemerBurn :: IO ()
writeRedeemerBurn = 

    let red = Scripts.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = False  -- burn token
                     }
    in
        writeJSON "deploy/redeemer-token-burn.json" red


writeTokenName :: IO ()
writeTokenName = writeJSON "deploy/token-name.json" tokenName


writeTokenMetadata :: IO ()
writeTokenMetadata = 
    
    let file = "deploy/token-metadata.json"
    in LBS.writeFile file $ encode tokenMetadata
    
writeMintingPolicyHash :: IO ()
writeMintingPolicyHash = writeJSON "deploy/minting-policy.hash" $ PlutusTx.toBuiltinData $ Scripts.mintingPolicyHash $ policy mintParams

-- Pull out a validator from a minting policy
mintValidator :: Ledger.MintingPolicy -> Ledger.Validator
mintValidator pol = Ledger.Validator $ unMintingPolicyScript pol

writeMintingPolicy :: IO (Either (FileError ()) ())
writeMintingPolicy = writeValidator "deploy/minting-policy.plutus" $ mintValidator $ policy mintParams

  


  
