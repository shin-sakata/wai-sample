module Core
  ( listenAndServe
  ) where

import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           RIO
import           Router2                  (Routes, router)

app :: Routes -> Application
app routes request respond = do
  handler <- runRIO request (router request routes)
  respond handler

type Port = Int

listenAndServe :: Port -> Routes -> IO ()
listenAndServe port routes = run port $ app routes
