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


{-|
  curl "Content-Type: application/json" -d '{"name":"chemirea", "age": 20}' localhost:8080/user/echo
  >> {"age":20,"name":"chemirea"}
-}
echoUser :: Action
echoUser = do
  maybeUser <- decodeUser <$> (fromStrict <$> requestBodyChunk)
  maybeJson maybeUser


getQuery :: ByteString -> HasRequest (Maybe ByteString)
getQuery key = join <$> (lookup key <$> queryString)

users :: IO [User]
users = return [User "noname" 20, User "らむだ ファンタろう" 24, User "無職 やめたろう" 40]
