{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Config
import Control.Concurrent (threadDelay)
import Data.Aeson
  ( eitherDecode,
    encode,
  )
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.IORef
import qualified Data.Map.Lazy as Map
import LocationData
import Network.HTTP.Simple
  ( Request,
    Response,
    getResponseBody,
    httpLBS,
    parseRequest_,
  )

data Environment = Environment
  { locationDataLs :: Map.Map Location [LocationData],
    delay :: Int,
    connectInfo :: ConnectInfo
  }
  deriving (Show)

data ConnectInfo = ConnectInfo
  { domain_ :: String,
    key_ :: String
  }
  deriving (Show)

mkEnvironment :: Configuration -> Environment
mkEnvironment conf = do
  let locationLs = locations conf
      ls = Map.fromList . zip locationLs $ replicate (length locationLs) []
  Environment
    { locationDataLs = ls,
      delay = timeDelay conf,
      connectInfo = mkConnectInfo conf
    }

mkConnectInfo :: Configuration -> ConnectInfo
mkConnectInfo conf =
  ConnectInfo
    { domain_ = domain conf,
      key_ = key conf
    }

cachingLoop :: IORef Environment -> IO ()
cachingLoop ioRef = do
  env <- readIORef ioRef

  pure ()

buildGetRequest :: ConnectInfo -> Location -> Request
buildGetRequest connInf (Location str) =
  parseRequest_ . mconcat $
    [ "https://",
      domain_ connInf,
      "/data/2.5/weather?q=",
      str,
      "&appid=",
      key_ connInf
    ]

responseLs :: Configuration -> IO [LC.ByteString]
responseLs conf = do
  let ls = locations conf
      connInf = mkConnectInfo  conf
      resp = httpLBS . buildGetRequest connInf
  mapM (fmap getResponseBody . resp) ls

checkingWorkJSON :: ConnectInfo -> IO ()
checkingWorkJSON connInf = do
  let x = httpLBS . buildGetRequest connInf $ Location "Vancouver" :: IO (Response LC.ByteString)
  resp <- fmap getResponseBody x
  case eitherDecode resp :: Either String LocationData of
    Right obj -> do
      print obj
      putStrLn $ replicate 50 'X'
      print $ encode obj
    Left e -> putStrLn e
