{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Bam (
  extractBzgf,
) where

import Codec.Compression.Zlib.Raw
import Control.Monad
import Data.Bam.Types
import Data.Bam.Utils
import Data.Binary.Get
import Data.ByteString.Lazy qualified as BL
import Data.Digest.CRC32
import Data.Int
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte

type Parser = Parsec Void BL.ByteString

pWord16le :: Maybe String -> Parser Word16
pWord16le n = runGet (getWord16le) <$> takeP n 2

pInt16le :: Maybe String -> Parser Int16
pInt16le n = runGet (getInt16le) <$> takeP n 2

pWord32le :: Maybe String -> Parser Word32
pWord32le n = runGet getWord32le <$> takeP n 4

pInt32le :: Maybe String -> Parser Int32
pInt32le n = runGet getInt32le <$> takeP n 4

pFloatle :: Maybe String -> Parser Float
pFloatle n = runGet getFloatle <$> takeP n 4

parseBzgfBlock :: Parser BL.ByteString
parseBzgfBlock = do
  _ <- string "\x1f\x8b\x08\04" -- bzgf magic
  _ <- takeP Nothing 6

  xlen <- fromIntegral <$> pWord16le (Just "XLEN")
  unless (xlen == 6) (fail "xlen not 6")

  _ <- string "BC\x02\x00"

  bsize <- pWord16le (Just "BSIZE")
  deflate <- decompress <$> takeP (Just "CDATA") (fromIntegral bsize - xlen - 19)

  crc <- pWord32le (Just "CRC32")
  unless (crc == crc32 deflate) (fail "CRC32 checksum mismatch")

  _ <- takeP Nothing 4

  return $ deflate

parseBzgf :: Parser BL.ByteString
parseBzgf = BL.concat <$> many parseBzgfBlock <* eof

pRef :: Parser Ref
pRef = do
  l_name <- pWord32le Nothing
  name <- takeP (Just "ref_name") (fromIntegral l_name)
  l_ref <- pWord32le (Just "l_ref")

  return (name, fromIntegral l_ref)

pTag :: Parser (BL.ByteString, TagValue)
pTag = do
  tag <- takeP (Just "tag") 2

  val_type <- anySingle

  (tag,) <$> case val_type of
    -- A
    65 -> BSValue <$> takeP Nothing 1
    -- c
    99 -> Int8Value . fromIntegral <$> anySingle
    -- C
    67 -> Word8Value . fromIntegral <$> anySingle
    -- s
    115 -> Int16Value <$> pInt16le Nothing
    -- S
    83 -> Word16Value <$> pWord16le Nothing
    -- i
    105 -> Int32Value <$> pInt32le Nothing
    -- I
    73 -> Word32Value <$> pWord32le Nothing
    -- f
    102 -> FloatValue <$> pFloatle Nothing
    -- Z
    90 -> BSValue <$> takeWhileP Nothing (/= 0) <* char 0
    -- H
    72 -> BSValue <$> takeWhileP Nothing (/= 0) <* char 0
    -- B
    66 -> do
      t <- anySingle
      cnt <- fromIntegral <$> pWord32le Nothing
      case t of
        99 -> Int8ListValue . fmap fromIntegral . BL.unpack <$> takeP Nothing cnt
        67 -> Word8ListValue . fmap fromIntegral . BL.unpack <$> takeP Nothing cnt
        115 -> Int16ListValue <$> count cnt (pInt16le Nothing)
        83 -> Word16ListValue <$> count cnt (pWord16le Nothing)
        105 -> Int32ListValue <$> count cnt (pInt32le Nothing)
        73 -> Word32ListValue <$> count cnt (pWord32le Nothing)
        102 -> FloatListValue <$> count cnt (pFloatle Nothing)
        _ -> fail "bad value type"
    _ -> fail "bad value type"

pTags :: Int -> Parser [(BL.ByteString, TagValue)]
pTags end = do
  offset <- getOffset
  if (offset < end)
    then (:) <$> pTag <*> pTags end
    else return []

pBlock :: Refs -> Parser BL.ByteString
pBlock refs = do
  blockSize <- pWord32le (Just "block_size")

  ref <- (refs !!) . fromIntegral <$> pInt32le (Just "refID")

  pos <- pInt32le (Just "pos")

  l_read_name :: Word8 <- anySingle <?> "l_read_name"

  mapq :: Word8 <- anySingle <?> "mapq"

  bin <- pWord16le (Just "bin")

  n_cigar_op <- pWord16le (Just "n_cigar_op")
  flag <- pWord16le (Just "flag")
  l_seq <- pWord32le (Just "l_seq")
  next_refID <- pInt32le (Just "next_refID")
  next_pos <- pInt32le (Just "next_pos")
  tlen <- pInt32le (Just "tlen")
  read_name <- takeP (Just "read_name") (fromIntegral l_read_name)
  cigar <-
    cigarToPrintable <$> count (fromIntegral n_cigar_op) (pWord32le (Just "cigar"))
  code <-
    (`bin2code` l_seq) <$> takeP (Just "seq") ((fromIntegral l_seq + 1) `div` 2)
  qual <- qualToPrintable <$> takeP (Just "qual") (fromIntegral l_seq)

  start <- getOffset

  let end =
        start
          + ( fromIntegral blockSize
                - 32
                - fromIntegral l_read_name
                - fromIntegral n_cigar_op * 4
                - ((fromIntegral l_seq + 1) `div` 2)
                - fromIntegral l_seq
            )

  tags <- pTags end

  return code

parseBam :: Parser [BL.ByteString]
parseBam = do
  _ <- string "BAM\x01"
  l_text <- pWord32le (Just "l_text")

  header <- takeP (Just "text") (fromIntegral l_text)

  n_ref <- pWord32le (Just "n_ref")

  refs <- count (fromIntegral n_ref) pRef

  blocks <- many (pBlock refs)

  eof

  return blocks

decodeBam ::
  BL.ByteString -> (Either (ParseErrorBundle BL.ByteString Void) [BL.ByteString])
decodeBam bam = do
  parse parseBam "" bam

extractBzgf ::
  FilePath -> IO (Either (ParseErrorBundle BL.ByteString Void) [BL.ByteString])
extractBzgf fp = do
  content <- BL.readFile fp
  return $ do
    bamData <- parse parseBzgf fp content
    decodeBam bamData
