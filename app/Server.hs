{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Config (Location (Location))
import Control.Concurrent (MVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Maybe (isJust, listToMaybe)
import Environment
  ( Environment (connectInfo, locationsDataLs, marginTime),
  )
import Lib
  ( TimeStamp,
    mkGetRequest,
    mkMarginErrorTimeLs,
    nearestLocationData,
  )
import LocationData (LocationData (dt))
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
      let locDtLs = locationsDataLs env -- if need
          maybeLocationDataLs = lookup locationName locDtLs
          maybeLastLocationData = maybeLocationDataLs >>= listToMaybe
          lastUpdateTime = maybe 0 dt maybeLastLocationData
      if isJust maybeLastLocationData && timeStemp <= lastUpdateTime
        then do
          _ <- liftIO $ print "Done! ___________________________________________________" -- Delete
          let marginErrTime = marginTime env
              timeStempLs = mkMarginErrorTimeLs timeStemp marginErrTime
          _ <- liftIO $ print timeStempLs --                                                 Delete
          pure . maybeToEither $
            nearestLocationData maybeLocationDataLs timeStempLs
        else getEitherLocationData
    Nothing -> getEitherLocationData
  where
    maybeToEither maybeVal =
      case maybeVal of
        Just obj -> Right obj
        _ -> Left "There is no data for this location for this time interval"

api :: Proxy API
api = Proxy

server :: MVar Environment -> Server API
server = handler

app :: MVar Environment -> Application
app = serve api . server
