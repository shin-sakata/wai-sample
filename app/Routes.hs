module Routes
  ( route
  ) where

import           Controller                    (Action)
import           Controller.NotFoundController (notFound)
import           Controller.TopController      (top)
import           Controller.UsersController    (user)
import           Network.HTTP.Types
import           Network.Wai                   (Request, Response, pathInfo,
                                                requestMethod)
import           RIO

route :: Request -> IO Response
route req =
  case pathInfo req
    -- /
        of
    []       -> topRoute req
    -- /user
    ["user"] -> userRoute req
    -- Did not match
    _        -> runRIO req notFound

userRoute :: Request -> IO Response
userRoute req =
  case requestMethod req of
    "GET" -> runRIO req user
    _     -> runRIO req notFound

topRoute :: Request -> IO Response
topRoute req =
  case requestMethod req of
    "GET" -> runRIO req top
    _     -> runRIO req notFound
