{-# LANGUAGE OverloadedStrings #-}

module Data.Bam.Utils where

import Data.Bits
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Foldable
import Data.Word

qualToPrintable :: BL.ByteString -> BL.ByteString
qualToPrintable = BL.map (+ 33)

codeMap :: B.ByteString
codeMap = "=ACMGRSVTWYHKDBN"

bin2code :: (Integral a) => BL.ByteString -> a -> BL.ByteString
bin2code bin len = BL.take (fromIntegral len) code
 where
  code = BL.foldl' convert "" (BL.take ((fromIntegral len + 1) `div` 2) bin)
  convert acc w =
    acc
      `BL.snoc` (codeMap `B.index` fromIntegral (w `shiftR` 4))
      `BL.snoc` (codeMap `B.index` fromIntegral (w .&. 0x0f))

cigarMap :: B.ByteString
cigarMap = "MIDNSHP=X"

cigarToPrintable :: [Word32] -> BL.ByteString
cigarToPrintable bin = foldl' convert "" bin
 where
  convert acc w =
    acc
      <> (C8.pack . show) (w `shiftR` 4)
      `BL.snoc` (cigarMap `B.index` fromIntegral (w .&. 0x0f))
