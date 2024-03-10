{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Bam
import Data.ByteString.Lazy qualified as BL
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

  return ()
