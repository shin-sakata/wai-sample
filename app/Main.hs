module Main where

import           Network.Wai              (Application)
import qualified Network.Wai              as Wai
import           Network.Wai.Handler.Warp (run)
import           RIO
import           Routes                   (route)

main :: IO ()
main = run 8080 app

app :: Application
app request respond = do
  handler <- route request
  respond handler
