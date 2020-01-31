module Controller.TopController where

import           Controller
import           Network.HTTP.Types
import           RIO

top :: Action
top = return $ 
  responseLBS 
    ok200
    [("Content-Type", "text/plain")]
    "Hello, world!"
