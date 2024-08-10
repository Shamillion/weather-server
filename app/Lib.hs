{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Config
import Control.Concurrent (MVar, putMVar, takeMVar, threadDelay)
import Data.Aeson
  ( decode,
    eitherDecode,
    encode,
  )
import qualified Data.ByteString.Lazy.Char8 as LC
--import qualified Data.Map.Lazy as Map

import Data.Maybe (fromMaybe)
import LocationData
import Network.HTTP.Simple
  ( Request,
    Response,
    getResponseBody,
    httpLBS,
    parseRequest_,
  )

data Environment = Environment
  { locationsDataLs :: [LocationDataTuple],
    delay :: Int,
    marginTime :: Int,
    connectInfo :: ConnectInfo
  }
  deriving (Show)

type LocationDataTuple = (Location, [LocationData])

data ConnectInfo = ConnectInfo
  { domain_ :: String,
    key_ :: String
  }
  deriving (Show)

mkEnvironment :: Configuration -> Environment
mkEnvironment conf = do
  let locationLs = locations conf
      ls = map (,[]) locationLs
  Environment
    { locationsDataLs = ls,
      delay = timeDelay conf,
      marginTime = marginErrorTime conf,
      connectInfo = mkConnectInfo conf
    }

mkConnectInfo :: Configuration -> ConnectInfo
mkConnectInfo conf =
  ConnectInfo
    { domain_ = fromMaybe "api.openweathermap.org" $ domain conf,
      key_ = key conf
    }

cachingLoop :: MVar Environment -> IO ()
cachingLoop mVar = do
  env <- takeMVar mVar
  let ls = locationsDataLs env
      connInf = connectInfo env
      pause = delay env
  newLs <- mapM (updateLocationDataLs connInf) ls
  putMVar mVar $ env {locationsDataLs = newLs}
  print . head . snd . head $ newLs --                                       <-- for test, delete
  threadDelay $ pause * 10 ^ (6 :: Int)
  cachingLoop mVar

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

getLocationData :: ConnectInfo -> Location -> IO (Maybe LocationData)
getLocationData connInf loc = do
  let req = buildGetRequest connInf loc
  decode . getResponseBody <$> httpLBS req

updateLocationDataLs :: ConnectInfo -> LocationDataTuple -> IO LocationDataTuple
updateLocationDataLs connInf (l, ls) = do
  maybeLocationData <- getLocationData connInf l
  let newLs =
        case maybeLocationData of
          Just locationData -> locationData : ls
          _ -> ls
  pure (l, newLs)

responseLs :: Configuration -> IO [LC.ByteString]
responseLs conf = do
  let ls = locations conf
      connInf = mkConnectInfo conf
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
