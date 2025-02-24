{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}

module Crypto.KDF.PBKDF where

import Data.Bits ((.>>.), (.&.))
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Word (Word32, Word64)

-- NB following synonym really only exists to make haddocks more
--    readable

-- | A HMAC function, taking a key as the first argument and the input
--   value as the second, producing a MAC digest.
--
--   >>> import qualified Crypto.Hash.SHA256 as SHA256
--   >>> :t SHA256.hmac
--   SHA256.hmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
--   >>> SHA256.hmac "my HMAC key" "my HMAC input"
--   <256-bit MAC>
type HMAC = BS.ByteString -> BS.ByteString -> BS.ByteString

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- serialize a 32-bit word, MSB first
ser32 :: Word32 -> BS.ByteString
ser32 w =
  let !mask = 0b00000000_00000000_00000000_11111111
      !w0 = fi (w .>>. 24) .&. mask
      !w1 = fi (w .>>. 16) .&. mask
      !w2 = fi (w .>>. 08) .&. mask
      !w3 = fi w .&. mask
  in  BS.cons w0 (BS.cons w1 (BS.cons w2 (BS.singleton w3)))

-- bytewise xor on bytestrings
xor :: BS.ByteString -> BS.ByteString -> BS.ByteString
xor = BS.packZipWith B.xor
{-# INLINE xor #-}

-- pbkdf2
derive
  :: HMAC          -- ^ HMAC function
  -> BS.ByteString -- ^ password
  -> BS.ByteString -- ^ salt
  -> Word64        -- ^ iteration count
  -> Word32        -- ^ bytelength of derived key (max 0xffff_ffff * hlen)
  -> BS.ByteString -- ^ derived key
derive prf p s c dklen
    | dklen > 0xffff_ffff * fi hlen =      -- 2 ^ 32 - 1
        error "ppad-pbkdf (derive): derived key too long"
    | otherwise =
        loop mempty 1
  where
    !hlen = BS.length (prf mempty mempty)
    !l = ceiling (fi dklen / fi hlen :: Double) :: Word32
    !r = fi (dklen - (l - 1) * fi hlen)

    f !i =
      let go j !acc !las
            | j == c = acc
            | otherwise =
                let u = prf p las
                    nacc = acc `xor` u
                in  go (j + 1) nacc u

          org = prf p (s <> ser32 i)

      in  go 1 org org
    {-# INLINE f #-}

    loop !acc !i
      | i == l =
          let t = f i
              fin = BS.take r t
          in  BS.toStrict . BSB.toLazyByteString $
                acc <> BSB.byteString fin
      | otherwise =
          let t = f i
              nacc = acc <> BSB.byteString t
          in  loop nacc (i + 1)

