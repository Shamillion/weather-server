{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Config (Configuration (locations), Location (Location))
import Control.Applicative (Applicative (liftA2))
import Control.Concurrent (MVar, modifyMVar_)
import qualified Control.Concurrent.Thread.Delay as D
import Data.Aeson
  ( decode,
    eitherDecode,
    encode,
  )
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Foldable (find)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Environment
  ( ConnectInfo (domain_, key_),
    Environment (connectInfo, delay, locationsDataLs),
    LocationDataTuple,
    mkConnectInfo,
  )
import LocationData (Coord (..), LocationData (dt), RequestedLocation)
import Network.HTTP.Simple
  ( Request,
    Response,
    getResponseBody,
    httpLBS,
    parseRequest_,
  )
import Control.Concurrent.MVar (readMVar)

cachingLoop :: MVar Environment -> IO ()
cachingLoop mVar = do
  env <- readMVar mVar
  let ls = locationsDataLs env
      connInf = connectInfo env
      pause = delay env
  newLs <- mapM (updateLocationDataLs connInf) ls
  modifyMVar_ mVar $ \_ -> pure env {locationsDataLs = newLs}
  D.delay $ pause * 10 ^ (6 :: Integer)
  cachingLoop mVar

mkGetRequest :: ConnectInfo -> Location -> Request
mkGetRequest connInf (Location str) =
  parseRequest_ . mconcat $
    [ "https://",
      domain_ connInf,
      "/data/2.5/weather?q=",
      str,
      "&appid=",
      key_ connInf
    ]

mkGetRequestCoord :: ConnectInfo -> Location -> Request
mkGetRequestCoord connInf (Location str) =
  parseRequest_ . mconcat $
    [ "https://",
      domain_ connInf,
      "/geo/1.0/direct?q=",
      str,
      "&appid=",
      key_ connInf
    ]

getLocationData :: ConnectInfo -> Location -> IO (Maybe LocationData)
getLocationData connInf loc = do
  let req = mkGetRequest connInf loc
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
      resp = httpLBS . mkGetRequest connInf
  mapM (fmap getResponseBody . resp) ls

type TimeStamp = Int

type MarginErrorTime = Int

mkMarginErrorTimeLs :: TimeStamp -> MarginErrorTime -> [TimeStamp]
mkMarginErrorTimeLs timeStamp marginErrTime = do
  let tupleLs =
        zip [timeStamp + 1 .. timeStamp + marginErrTime] $
          reverse [timeStamp - marginErrTime .. timeStamp - 1]
  timeStamp : foldr (\(x, y) ls -> x : y : ls) [] tupleLs

nearestLocationData :: Maybe [LocationData] -> [TimeStamp] -> Maybe LocationData
nearestLocationData maybeLs timeLs
  | isNothing maybeLs || null timeLs || fmap null maybeLs == Just True = Nothing
  | otherwise = do
    let maybeLocationData = searchFitLocationDataByTime maybeLs timeLs
    if isJust maybeLocationData
      then maybeLocationData
      else do
        let lastLocationData = maybeLs >>= listToMaybe
            firstLocationData = maybeLs >>= listToMaybe . reverse
            timeStamp = listToMaybe timeLs
            lastLocationDataTime = fmap dt lastLocationData
            firstLocationDataTime = fmap dt firstLocationData
            diffLastLocationDataTime = liftA2 (\x y -> abs $ x - y) lastLocationDataTime timeStamp
            diffFirstLocationDataTime = liftA2 (\x y -> abs $ x - y) firstLocationDataTime timeStamp
        case liftA2 (>) diffLastLocationDataTime diffFirstLocationDataTime of
          Just True -> firstLocationData
          _ -> lastLocationData

searchFitLocationDataByTime :: Maybe [LocationData] -> [TimeStamp] -> Maybe LocationData
searchFitLocationDataByTime Nothing _ = Nothing
searchFitLocationDataByTime _ [] = Nothing
searchFitLocationDataByTime (Just ls) (x : xs) = do
  let maybeData = find ((x ==) . dt) ls
  if isJust maybeData
    then maybeData
    else searchFitLocationDataByTime (Just ls) xs

searchSuitableLocationDataByCoord :: Double -> Coord -> Coord -> Bool
searchSuitableLocationDataByCoord mrgnErrCoord coordReqLoc coordLd = do
  let maxLat = lat coordReqLoc + mrgnErrCoord
      minLat = lat coordReqLoc - mrgnErrCoord
      maxLon = lon coordReqLoc + mrgnErrCoord
      minLon = lon coordReqLoc - mrgnErrCoord
      latLocData = lat coordLd
      lonLocData = lon coordLd
  latLocData >= minLat && latLocData <= maxLat
    && lonLocData >= minLon
    && lonLocData <= maxLon

checkingWorkJSON :: ConnectInfo -> IO ()
checkingWorkJSON connInf = do
  let x = httpLBS . mkGetRequestCoord connInf $ Location "Vancouver" :: IO (Response LC.ByteString)
  resp <- fmap getResponseBody x
  print resp
  --case eitherDecode resp :: Either String LocationData of
  case eitherDecode resp :: Either String [RequestedLocation] of
    Right obj -> do
      print obj
      putStrLn $ replicate 50 'X'
      print $ encode obj
    Left e -> putStrLn e
