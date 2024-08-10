{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

-- import qualified Data.ByteString.Lazy.Char8 as LC
-- import qualified Network.Wai as W
import Config
import Control.Concurrent (MVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.List (find)
import Data.Maybe (isJust, listToMaybe)
import Lib
import LocationData
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
  )
import Servant

--import qualified Data.Text as T

type API =
  Capture "location" String
    :> QueryParam "timestamp" Int
    :> Get '[JSON] (Either String LocationData) -- change to Maybe

handler :: MVar Environment -> String -> Maybe Int -> Handler (Either String LocationData) -- change to Maybe
handler mVar loc maybeTime = do
  env <- liftIO $ readMVar mVar
  let locationName = Location loc
      connInf = connectInfo env
      getEitherLocationData =
        eitherDecode . getResponseBody
          <$> httpLBS (buildGetRequest connInf locationName) -- change to decode
  _ <- liftIO $ print (loc, maybeTime)
  case maybeTime of
    Just time -> do
      let locDtLs = locationsDataLs env -- if need
          maybeLocationDataLs = lookup locationName locDtLs
          maybeLastLocationData = maybeLocationDataLs >>= listToMaybe
          lastUpdateTime = maybe 0 dt maybeLastLocationData
      if isJust maybeLastLocationData && time <= lastUpdateTime
        then do
          pure . maybeToEither $
            maybeLocationDataLs >>= find ((time ==) . dt)
        else getEitherLocationData
    Nothing -> getEitherLocationData
  where
    maybeToEither maybeVal =
      case maybeVal of
        Just obj -> Right obj
        _ -> Left "Error in handler"

api :: Proxy API
api = Proxy

server :: MVar Environment -> Server API
server = handler

app :: MVar Environment -> Application
app = serve api . server
