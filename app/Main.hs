{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Bam
import Data.ByteString qualified as B
import Options.Applicative

data BamConfigs = BamConfigs
  { file :: FilePath
  }

parser :: Parser BamConfigs
parser = BamConfigs <$> strArgument (metavar "FILE")

main :: IO ()
main = do
  BamConfigs{..} <- execParser $ info (parser <**> helper) fullDesc

  extractBzgf file >>= \case
    Left e -> print e
    Right recs -> B.putStr $ B.intercalate "\n" recs
