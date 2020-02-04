module Routes
  ( routes
  ) where

import           Controller.TopController   (top)
import           Controller.UsersController (createUser, user)
import           RIO
import           Router2                    (Routes, get, int, match, text,
                                             (/>), (</), (</>))

routes :: Routes
routes = do
  get (match "/") (const top)
  get (match "/user" /> int) user
  get (match "/createUser" /> text </ text </> int) createUser
