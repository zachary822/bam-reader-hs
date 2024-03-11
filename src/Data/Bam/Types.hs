module Data.Bam.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Word

data TagValue
  = Int8Value Int8
  | Word8Value Word8
  | Int16Value Int16
  | Word16Value Word16
  | Int32Value Int32
  | Word32Value Word32
  | FloatValue Float
  | BSValue ByteString
  | Int8ListValue [Int8]
  | Word8ListValue [Word8]
  | Int16ListValue [Int16]
  | Word16ListValue [Word16]
  | Int32ListValue [Int32]
  | Word32ListValue [Word32]
  | FloatListValue [Float]
  deriving (Show, Eq)

type Ref = (ByteString, Integer)
type Refs = [Ref]
