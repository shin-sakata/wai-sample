{-# LANGUAGE DeriveAnyClass #-}

module Model.User where

import           Data.Aeson (FromJSON, ToJSON, decode)
import           RIO

data User =
  User
    { name :: Text
    , age  :: Int
    }
  deriving (Generic, Show, FromJSON, ToJSON)

decodeUser :: LByteString -> Maybe User
decodeUser = decode