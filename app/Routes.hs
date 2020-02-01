module Routes
  ( routes
  ) where

import           Controller.TopController   (top)
import           Controller.UsersController (user)
import           RIO
import           Router                     (Routes, get, post, routesWriter)

routes :: Routes
routes = do
  get "/" top
  get "/user" user
