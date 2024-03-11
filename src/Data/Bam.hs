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
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Digest.CRC32
import Data.Void
import Data.Word
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary

type Parser = Parsec Void ByteString

floatle :: Parser Float
floatle = do
  runGet (getFloatle) . BL.fromStrict <$> takeP (Just "32-bit floating point") 4

parseBzgfBlock :: Parser ByteString
parseBzgfBlock = do
  _ <- string "\x1f\x8b\x08\04" -- bzgf magic
  _ <- takeP Nothing 6

  xlen <- fromIntegral <$> word16le
  unless (xlen == 6) (fail "xlen not 6")

  _ <- string "BC\x02\x00"

  bsize <- word16le
  deflate <-
    BL.toStrict . decompress . BL.fromStrict
      <$> takeP (Just "CDATA") (fromIntegral bsize - xlen - 19)

  crc <- word32le
  unless (crc == crc32 deflate) (fail "CRC32 checksum mismatch")

  _ <- takeP Nothing 4

  return deflate

parseBzgf :: Parser ByteString
parseBzgf = B.concat <$> many parseBzgfBlock <* eof

pRef :: Parser Ref
pRef = do
  l_name <- word32le
  name <- takeP (Just "ref_name") (fromIntegral l_name)
  l_ref <- word32le

  traceShowM l_ref

  return (name, fromIntegral l_ref)

pTag :: Parser (ByteString, TagValue)
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
    115 -> Int16Value <$> int16le
    -- S
    83 -> Word16Value <$> word16le
    -- i
    105 -> Int32Value <$> int32le
    -- I
    73 -> Word32Value <$> word32le
    -- f
    102 -> FloatValue <$> floatle
    -- Z
    90 -> BSValue <$> takeWhileP Nothing (/= 0) <* char 0
    -- H
    72 -> BSValue <$> takeWhileP Nothing (/= 0) <* char 0
    -- B
    66 -> do
      t <- anySingle

      cnt <- fromIntegral <$> word32le
      case t of
        99 -> Int8ListValue . fmap fromIntegral . B.unpack <$> takeP Nothing cnt
        67 -> Word8ListValue . fmap fromIntegral . B.unpack <$> takeP Nothing cnt
        115 -> Int16ListValue <$> count cnt int16le
        83 -> Word16ListValue <$> count cnt word16le
        105 -> Int32ListValue <$> count cnt int32le
        73 -> Word32ListValue <$> count cnt word32le
        102 -> FloatListValue <$> count cnt floatle
        _ -> fail "bad value type"
    _ -> fail "bad value type"

pTags :: Int -> Parser [(ByteString, TagValue)]
pTags end = do
  offset <- getOffset
  if (offset < end)
    then (:) <$> pTag <*> pTags end
    else return []

pBlock :: Refs -> Parser ByteString
pBlock refs = do
  blockSize <- word32le

  ref <- (refs !!) . fromIntegral <$> int32le

  pos <- int32le

  l_read_name :: Word8 <- anySingle <?> "l_read_name"

  mapq :: Word8 <- anySingle <?> "mapq"

  bin <- word16le

  n_cigar_op <- word16le
  flag <- word16le
  l_seq <- word32le
  next_refID <- int32le
  next_pos <- int32le
  tlen <- int32le
  read_name <- takeP (Just "read_name") (fromIntegral l_read_name)
  cigar <-
    cigarToPrintable <$> count (fromIntegral n_cigar_op) word32le
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

parseBam :: Parser [ByteString]
parseBam = do
  _ <- string "BAM\x01"
  l_text <- word32le

  header <- takeP (Just "text") (fromIntegral l_text)

  n_ref <- word32le

  refs <- count (fromIntegral n_ref) pRef

  blocks <- many (pBlock refs)

  eof

  return blocks

decodeBam ::
  ByteString -> (Either (ParseErrorBundle ByteString Void) [ByteString])
decodeBam bam = do
  parse parseBam "" bam

extractBzgf ::
  FilePath -> IO (Either (ParseErrorBundle ByteString Void) [ByteString])
extractBzgf fp = do
  content <- B.readFile fp
  return $ do
    bamData <- parse parseBzgf fp content
    decodeBam bamData
