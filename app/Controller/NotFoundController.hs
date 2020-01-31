module Controller.NotFoundController
  ( notFound
  ) where

import           Controller
import           Network.HTTP.Types
import           Network.Wai        (Response, responseBuilder, responseLBS, Request)
import           RIO
import           RIO.ByteString.Lazy (fromStrict)

notFound :: Action
notFound = do
  path <- fromStrict <$> rawPathInfo
  return $ responseLBS notFound404 [("Content-Type", "text/plain")] (path <> " is Not Found 404")
