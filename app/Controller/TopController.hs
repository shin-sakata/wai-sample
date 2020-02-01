module Controller.TopController where

import           Controller
import           Network.HTTP.Types
import           RIO

top :: Action
top = responseText "Hello world!"
