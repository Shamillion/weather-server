{-# LANGUAGE TupleSections #-}

module Environment where

import Config
  ( Configuration (domain, key, locations, marginErrorCoordinate, marginErrorTime, timeDelay),
    Location,
    defaultDomain,
  )
import Data.Maybe (fromMaybe)
import LocationData (LocationData)

data Environment = Environment
  { locationsDataLs :: [LocationDataTuple],
    delay :: Integer,
    marginErrTime :: Int,
    marginErrCoordinate :: Double,
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
      marginErrTime = marginErrorTime conf,
      marginErrCoordinate = marginErrorCoordinate conf,
      connectInfo = mkConnectInfo conf
    }

mkConnectInfo :: Configuration -> ConnectInfo
mkConnectInfo conf =
  ConnectInfo
    { domain_ = fromMaybe defaultDomain $ domain conf,
      key_ = key conf
    }
