{-# LANGUAGE TupleSections #-}

module Environment where

import Config
  ( Configuration (domain, key, locations, marginErrorTime, timeDelay),
    Location,
  )
import Data.Maybe (fromMaybe)
import LocationData (LocationData)

data Environment = Environment
  { locationsDataLs :: [LocationDataTuple],
    delay :: Integer,
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
