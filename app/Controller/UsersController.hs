module Controller.UsersController
  ( user
  ) where

import           Controller
import           Model.User
import           RIO

user :: Action
user = do
  let user = User {name = "ほげほげ君", age = 17}
  return $ responseJson user
