{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Config (Location (Location))
import Control.Concurrent (MVar, readMVar)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, eitherDecode)
import Data.Maybe (isJust, listToMaybe)
import Environment
  ( Environment
      ( connectInfo,
        locationsDataLs,
        marginErrCoordinate,
        marginErrTime
      ),
  )
import Lib
  ( TimeStamp,
    mkGetRequest,
    mkGetRequestCoord,
    mkMarginErrorTimeLs,
    nearestLocationData,
    searchSuitableLocationDataByCoord,
  )
import LocationData
  ( LocationData (coord, dt),
    RequestedLocation (coordRL),
  )
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
  )
import Servant
  ( Application,
    Capture,
    Get,
    Handler,
    JSON,
    Proxy (..),
    QueryParam,
    Server,
    serve,
    type (:>),
  )

type API =
  Capture "location" String
    :> QueryParam "timestamp" Int
    :> Get '[JSON] (Either String LocationData) -- change to Maybe

handler :: MVar Environment -> String -> Maybe TimeStamp -> Handler (Either String LocationData) -- change to Maybe
handler mVar loc maybeTimeStamp = do
  env <- liftIO $ readMVar mVar
  let locationName = Location loc
      connInf = connectInfo env
      getEitherLocationData =
        eitherDecode . getResponseBody
          <$> httpLBS (mkGetRequest connInf locationName) -- change to decode
  case maybeTimeStamp of
    Just timeStemp -> do
      let locDtTupleLs = locationsDataLs env -- if need
          maybeLocationDataLs = lookup locationName locDtTupleLs
          maybeLastLocationData = maybeLocationDataLs >>= listToMaybe
          lastUpdateTime = maybe 0 dt $ listToMaybe locDtTupleLs >>= listToMaybe . snd
          lastUpdateTimeLocationData = maybe 0 dt maybeLastLocationData
          mrgnErrTime = marginErrTime env
          marginErrTimeExtended = max mrgnErrTime 500
          timeStempLs = mkMarginErrorTimeLs timeStemp marginErrTimeExtended
      if isJust maybeLastLocationData && timeStemp <= lastUpdateTimeLocationData
        then pure . maybeToEither $ nearestLocationData maybeLocationDataLs timeStempLs
        else
          if timeStemp <= lastUpdateTime
            then do
              resp <- httpLBS (mkGetRequestCoord connInf locationName)
              let maybeLocationData = decode $ getResponseBody resp :: Maybe [RequestedLocation]
                  maybeCoordRequestedLocation = coordRL <$> (listToMaybe =<< maybeLocationData)
                  mrgnErrCoordinate = marginErrCoordinate env
                  locationDataLs = map snd locDtTupleLs
                  coordCompare = searchSuitableLocationDataByCoord mrgnErrCoordinate
                  fitLocationDataLs = do
                    coordRequestedLocation <- maybeCoordRequestedLocation
                    listToMaybe
                      =<< filterM
                        (fmap (coordCompare coordRequestedLocation . coord) . listToMaybe)
                        locationDataLs
                  maybeResLocationData = nearestLocationData fitLocationDataLs timeStempLs
              choiceObj maybeResLocationData getEitherLocationData
            else getEitherLocationData
    Nothing -> getEitherLocationData
  where
    maybeToEither maybeVal =
      case maybeVal of
        Just obj -> Right obj
        _ -> Left "There is no data for this location for this time interval"
    choiceObj maybeObj handlerObj =
      case maybeObj of
        Just _ -> pure $ maybeToEither maybeObj
        _ -> handlerObj

api :: Proxy API
api = Proxy

server :: MVar Environment -> Server API
server = handler

app :: MVar Environment -> Application
app = serve api . server
