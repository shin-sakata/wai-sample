module Main where

import           RIO
import           Routes (routes)

import           Core   (listenAndServe)

main :: IO ()
main = listenAndServe 8080 routes
