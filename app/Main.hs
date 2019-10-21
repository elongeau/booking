module Main where

import Network.Wai.Handler.Warp
import App

main :: IO ()
main = do
  connectionInfo <- readConnectionInfo
  pool <- mkDbPool connectionInfo
  let handle = mkHandle pool
  app <- application handle
  run 3000 app

