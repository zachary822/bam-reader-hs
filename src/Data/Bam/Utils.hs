{-# LANGUAGE OverloadedStrings #-}

module Data.Bam.Utils where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.Foldable
import Data.Word

qualToPrintable :: ByteString -> ByteString
qualToPrintable = B.map (+ 33)

codeMap :: ByteString
codeMap = "=ACMGRSVTWYHKDBN"

bin2code :: (Integral a) => ByteString -> a -> ByteString
bin2code bin len = B.take (fromIntegral len) code
 where
  code = B.foldl' convert "" (B.take ((fromIntegral len + 1) `div` 2) bin)
  convert acc w =
    acc
      `B.snoc` (codeMap `B.index` fromIntegral (w `shiftR` 4))
      `B.snoc` (codeMap `B.index` fromIntegral (w .&. 0x0f))

cigarMap :: ByteString
cigarMap = "MIDNSHP=X"

cigarToPrintable :: [Word32] -> ByteString
cigarToPrintable bin = foldl' convert "" bin
 where
  convert acc w =
    acc
      <> (C8.pack . show) (w `shiftR` 4)
      `B.snoc` (cigarMap `B.index` fromIntegral (w .&. 0x0f))
