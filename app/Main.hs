{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Data.Bam
import Options.Applicative

data BamConfigs = BamConfigs
  { file :: FilePath
  }

parser :: Parser BamConfigs
parser = BamConfigs <$> strArgument (metavar "FILE")

main :: IO ()
main = do
  BamConfigs{..} <- execParser $ info (parser <**> helper) fullDesc

  thing <- extractBzgf file

  _ <- evaluate thing

  return ()
