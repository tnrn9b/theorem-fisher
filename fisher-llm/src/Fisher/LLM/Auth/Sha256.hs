-- | SHA-256 (FIPS 180-4), for the two places the credential code hashes
-- short strings: the per-home gateway service name and token fingerprints.
-- Written out here so the package needs no hashing dependency for it.
module Fisher.LLM.Auth.Sha256
  ( sha256
  , sha256Hex
  ) where

import Data.Bits (complement, rotateR, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64, Word8)
import Numeric (showHex)

k256 :: [Word32]
k256 =
  [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
  , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
  , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
  , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
  , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
  , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
  , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
  , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2 ]

h0 :: [Word32]
h0 = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]

pad :: BS.ByteString -> [Word8]
pad bs =
  let len = BS.length bs
      bits = fromIntegral len * 8 :: Word64
      zeros = (55 - len) `mod` 64
   in BS.unpack bs <> [0x80] <> replicate zeros 0 <> [fromIntegral (bits `shiftR` (8 * i)) | i <- [7, 6 .. 0]]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (a, b) = splitAt n xs in a : chunks n b

word :: [Word8] -> Word32
word = foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0

block :: [Word32] -> [Word8] -> [Word32]
block hs bytes =
  let ws0 = map word (chunks 4 bytes)
      ws = take 64 (expand ws0)
      expand w = w <> go w (length w)
        where
          go acc i
            | i >= 64 = []
            | otherwise =
                let w15 = acc !! (i - 15)
                    w2 = acc !! (i - 2)
                    s0 = rotateR w15 7 `xor` rotateR w15 18 `xor` shiftR w15 3
                    s1 = rotateR w2 17 `xor` rotateR w2 19 `xor` shiftR w2 10
                    new = (acc !! (i - 16)) + s0 + (acc !! (i - 7)) + s1
                 in new : go (acc <> [new]) (i + 1)
      step [a, b, c, d, e, f, g, h] (kt, wt) =
        let s1 = rotateR e 6 `xor` rotateR e 11 `xor` rotateR e 25
            ch = (e .&. f) `xor` (complement e .&. g)
            t1 = h + s1 + ch + kt + wt
            s0 = rotateR a 2 `xor` rotateR a 13 `xor` rotateR a 22
            maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
            t2 = s0 + maj
         in [t1 + t2, a, b, c, d + t1, e, f, g]
      step st _ = st
      final = foldl' step hs (zip k256 ws)
   in zipWith (+) hs final

-- | The 32-byte digest.
sha256 :: BS.ByteString -> BS.ByteString
sha256 bs =
  let hs = foldl' block h0 (chunks 64 (pad bs))
   in BS.pack (concatMap (\w -> [fromIntegral (w `shiftR` s) | s <- [24, 16, 8, 0]]) hs)

-- | @hashlib.sha256(bs).hexdigest()@.
sha256Hex :: BS.ByteString -> Text
sha256Hex = T.pack . concatMap (\b -> let s = showHex b "" in if length s == 1 then '0' : s else s) . BS.unpack . sha256
