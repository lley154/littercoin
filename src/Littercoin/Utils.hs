{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Littercoin.Utils
  ( 
    integerToBS,
    decodeHex
  )
where


import qualified  Data.ByteString.Char8 as B        (ByteString)         
import qualified  Data.ByteString.Base16 as B16     (decode)
import            PlutusTx.Prelude                  (BuiltinByteString, consByteString, emptyByteString, Either (Left, Right), Integer, negate, 
                                                     otherwise, quotient, remainder, toBuiltin, (<), ($), (==), (<>), (+))
import            Prelude as Haskell                (String)


minusAsciiCode :: Integer
minusAsciiCode = 45

zeroAsciiCode :: Integer
zeroAsciiCode = 48

-- | Convert and Integer to a BuiltinByteString both offchain and onchain
{-# INLINEABLE integerToBS #-}
integerToBS :: Integer -> BuiltinByteString
integerToBS x
  | x < 0 = consByteString minusAsciiCode $ integerToBS (negate x)
  -- x is single-digit
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (d + zeroAsciiCode) emptyByteString


-- | Decode from hex base 16 to a base 10 bytestring is needed because
--   this is how it is stored in the ledger onchain
decodeHex :: B.ByteString -> BuiltinByteString
decodeHex hexBS =    
         case getTx of
            Right decHex -> do
                toBuiltin(decHex)  
            Left _ -> do
                emptyByteString 
                
        where        
            getTx :: Either Haskell.String B.ByteString = B16.decode hexBS