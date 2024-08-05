{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Aeson (FromJSON)
import Data.Yaml (decodeFileEither)
import GHC.Generics (Generic)
import System.Exit (die)

data Configuration = Configuration
  { domain :: String,
    key :: String,
    locations :: [Location]
  }
  deriving (Show, Generic, FromJSON)

data Location = Location
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Show, Generic, FromJSON)

readConfigFile :: IO Configuration
readConfigFile = do
  content <- decodeFileEither "config.yaml"
  case content of
    Right conf -> pure conf
    Left err -> do
      print err
      die "Error reading the configuration file! Check out config.yaml!"
