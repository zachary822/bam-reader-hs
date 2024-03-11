{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Bam.Utils
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Data.Bam.bin2code" $ do
    it "decode 0 bytes" $ do
      bin2code "" (0 :: Integer) `shouldBe` ""

    it "decodes odd codes" $ do
      bin2code "\x01\x23" (3 :: Integer) `shouldBe` "=AC"

    it "decodes even codes" $ do
      bin2code "\x01\x23" (4 :: Integer) `shouldBe` "=ACM"

    it "ignores extra bytes" $ do
      bin2code "\x01\x23" (2 :: Integer) `shouldBe` "=A"

    it "decodes all bytes if len is too long (even)" $ do
      bin2code "\x01\x23" (10 :: Integer) `shouldBe` "=ACM"

    it "decodes all bytes if len is too long (odd)" $ do
      bin2code "\x01\x23" (9 :: Integer) `shouldBe` "=ACM"
