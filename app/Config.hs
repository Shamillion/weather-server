{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson (FromJSON, ToJSON)
import Data.Yaml (decodeFileEither)
import GHC.Generics (Generic)
import System.Exit (die)

data Configuration = Configuration
  { serverPort :: Int,
    domain :: String,
    key :: String,
    timeDelay :: Int,
    locations :: [Location]
  }
  deriving (Show, Generic, FromJSON)

newtype Location = Location
  {location :: String}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

readConfigFile :: IO Configuration
readConfigFile = do
  content <- decodeFileEither "config.yaml"
  case content of
    Right conf -> pure conf
    Left err -> do
      print err
      die "Error reading the configuration file! Check out config.yaml!"
