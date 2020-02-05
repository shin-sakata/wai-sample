module Utf8Convertible where

import qualified Codec.Binary.UTF8.String as UTF8String
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy as LT
import Prelude (String, id, (.))

type Text = T.Text

type LText = LT.Text

type ByteString = BS.ByteString

type LByteString = LBS.ByteString


-- | Utf8Convertible class

class Utf8Convertible a b where
  convert :: a -> b

-- |
--------------------------------
-- from String instances
--------------------------------

instance Utf8Convertible String String where
  {-# INLINE convert #-}
  convert = id

instance Utf8Convertible String Text where
  {-# INLINE convert #-}
  convert = T.pack

instance Utf8Convertible String LText where
  {-# INLINE convert #-}
  convert = LT.pack

instance Utf8Convertible String ByteString where
  {-# INLINE convert #-}
  convert = BS.pack . UTF8String.encode

instance Utf8Convertible String LByteString where
  {-# INLINE convert #-}
  convert = LBS.pack . UTF8String.encode


-- |
--------------------------------
-- from Text instances
--------------------------------

instance Utf8Convertible Text String where
  {-# INLINE convert #-}
  convert = T.unpack

instance Utf8Convertible Text Text where
  {-# INLINE convert #-}
  convert = id

instance Utf8Convertible Text LText where
  {-# INLINE convert #-}
  convert = LT.fromStrict

instance Utf8Convertible Text ByteString where
  {-# INLINE convert #-}
  convert = E.encodeUtf8

instance Utf8Convertible Text LByteString where
  {-# INLINE convert #-}
  convert = LBS.fromStrict . convert


-- |
--------------------------------
-- from LazyText instances
--------------------------------

instance Utf8Convertible LText String where
  {-# INLINE convert #-}
  convert = LT.unpack

instance Utf8Convertible LText Text where
  {-# INLINE convert #-}
  convert = LT.toStrict

instance Utf8Convertible LText LText where
  {-# INLINE convert #-}
  convert = id

instance Utf8Convertible LText ByteString where
  {-# INLINE convert #-}
  convert = LBS.toStrict . LE.encodeUtf8

instance Utf8Convertible LText LByteString where
  {-# INLINE convert #-}
  convert = LE.encodeUtf8


-- |
--------------------------------
-- from ByteString instances
--------------------------------

instance Utf8Convertible ByteString String where
  {-# INLINE convert #-}
  convert = UTF8String.decode . BS.unpack

instance Utf8Convertible ByteString Text where
  {-# INLINE convert #-}
  convert = E.decodeUtf8

instance Utf8Convertible ByteString LText where
  {-# INLINE convert #-}
  convert = LT.fromStrict . E.decodeUtf8

instance Utf8Convertible ByteString ByteString where
  {-# INLINE convert #-}
  convert = id

instance Utf8Convertible ByteString LByteString where
  {-# INLINE convert #-}
  convert = LBS.fromStrict

-- |
--------------------------------
-- from LazyByteString instances
--------------------------------

instance Utf8Convertible LByteString String where
  {-# INLINE convert #-}
  convert = UTF8String.decode . LBS.unpack

instance Utf8Convertible LByteString Text where
  {-# INLINE convert #-}
  convert = LT.toStrict . LE.decodeUtf8

instance Utf8Convertible LByteString LText where
  {-# INLINE convert #-}
  convert = LE.decodeUtf8

instance Utf8Convertible LByteString ByteString where
  {-# INLINE convert #-}
  convert = LBS.toStrict

instance Utf8Convertible LByteString LByteString where
  {-# INLINE convert #-}
  convert = id
