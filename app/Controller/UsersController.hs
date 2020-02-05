module Controller.UsersController
  ( user
  , echoUser
  ) where

import           Controller
import           Data.Maybe          (fromMaybe)
import           Model.User
import           Prelude             ((!!))
import           RIO
import           RIO.ByteString.Lazy (fromStrict)
import           RIO.HashMap         (fromList)

user :: Int -> Action
user id = do
  users_ <- liftIO users
  let user = users_ !! id
  json user


echoUser :: (Text, Int) -> Action
echoUser (name, age) = json $ User name age


getQuery :: ByteString -> HasRequest (Maybe ByteString)
getQuery key = join <$> (lookup key <$> queryString)

users :: IO [User]
users = return [User "noname" 20, User "ほげ　太郎" 24, User "ふが　花子" 40]
