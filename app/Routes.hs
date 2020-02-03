module Routes
  ( routes
  ) where

import           Controller.TopController   (top)
import           Controller.UsersController (echoUser, user)
import           RIO
import           Router2                    (Routes, get, int, match, post,
                                             text)

routes :: Routes
routes = do
  get (match "/") (const top)
  get (match "/user/" >> int) user
  post (match "/user/echo") (const echoUser)
