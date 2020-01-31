module Model.User where

import           Data.Aeson (FromJSON, ToJSON)
import           RIO

data User =
  User
    { name :: Text
    , age  :: Int
    }
  deriving (Generic, Show)

instance ToJSON User

instance FromJSON User
