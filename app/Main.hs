{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Config (Configuration (serverPort), readConfigFile)
import Control.Concurrent (forkIO, newMVar)
import Lib (cachingLoop, mkEnvironment)
import Network.Wai.Handler.Warp (run)
import Server (app)

main :: IO ()
main = do
  conf <- readConfigFile
  mVar <- newMVar $ mkEnvironment conf
  _ <- forkIO $ do
    putStrLn "Caching is started."
    cachingLoop mVar
  let port = serverPort conf
  putStrLn "Server is started."
  run port $ app mVar

-- responseLs conf >>= print
-- print . locationDataLs . mkEnvironment $ conf
-- print conf
-- print $ mkConnectInfo conf
