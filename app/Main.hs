module Main where

import App
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  connectionInfo <- readConnectionInfo
  pool <- mkDbPool connectionInfo
  let handle = mkHandle pool
  app <- application handle
  run 8080 app
