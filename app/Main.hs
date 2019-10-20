module Main where

import Network.Wai.Handler.Warp
import App

main :: IO ()
main = do
  app <- application
  run 3000 app

