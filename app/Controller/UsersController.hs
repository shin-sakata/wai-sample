module Controller.UsersController
  ( user
  ) where

import           Controller
import           Data.Maybe         (fromMaybe)
import           Data.Text.Encoding (decodeUtf8With)
import           Model.User
import           Prelude            ((!!))
import           RIO
import           RIO.HashMap        (fromList)

user :: Int -> Action
user id = do
  users_ <- liftIO users
  let user = users_ !! id
  responseJson user

getQuery :: ByteString -> HasRequest (Maybe ByteString)
getQuery key = join <$> (lookup key <$> queryString)

users :: IO [User]
users = return [User "noname" 20, User "らむだ ファンタろう" 24, User "無職 やめたろう" 40]
