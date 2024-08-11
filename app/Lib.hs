{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Config
import Control.Applicative (Applicative (liftA2))
import Control.Concurrent (MVar, putMVar, takeMVar)
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
import LocationData (LocationData (dt))
import Network.HTTP.Simple
  ( Request,
    Response,
    getResponseBody,
    httpLBS,
    parseRequest_,
  )

cachingLoop :: MVar Environment -> IO ()
cachingLoop mVar = do
  env <- takeMVar mVar
  let ls = locationsDataLs env
      connInf = connectInfo env
      pause = delay env
  newLs <- mapM (updateLocationDataLs connInf) ls
  putMVar mVar $ env {locationsDataLs = newLs}
  print . dt . head . snd . head $ newLs --                                       <-- for test, delete
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
    let maybeLocationData = searchFitLocationData maybeLs timeLs
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

searchFitLocationData :: Maybe [LocationData] -> [TimeStamp] -> Maybe LocationData
searchFitLocationData Nothing _ = Nothing
searchFitLocationData _ [] = Nothing
searchFitLocationData (Just ls) (x : xs) = do
  let maybeData = find ((x ==) . dt) ls
  if isJust maybeData
    then maybeData
    else searchFitLocationData (Just ls) xs

checkingWorkJSON :: ConnectInfo -> IO ()
checkingWorkJSON connInf = do
  let x = httpLBS . mkGetRequest connInf $ Location "Vancouver" :: IO (Response LC.ByteString)
  resp <- fmap getResponseBody x
  case eitherDecode resp :: Either String LocationData of
    Right obj -> do
      print obj
      putStrLn $ replicate 50 'X'
      print $ encode obj
    Left e -> putStrLn e
