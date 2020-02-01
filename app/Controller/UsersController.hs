module Controller.UsersController
  ( user
  ) where

import           Controller
import           Data.Maybe         (fromMaybe)
import           Data.Text.Encoding (decodeUtf8With)
import           Model.User
import           RIO
import           RIO.HashMap        (fromList)

user :: Action
user = do
  name <- getQuery "name"
  let user = User {name = decodeUtf8With lenientDecode $ fromMaybe "noname" name, age = 20}
  responseJson user

getQuery :: ByteString -> HasRequest (Maybe ByteString)
getQuery key = join <$> (lookup key <$> queryString)
