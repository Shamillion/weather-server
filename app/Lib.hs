{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Object),
    eitherDecode,
    encode,
    object,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Lazy as Map
import Data.Yaml (decodeFileEither)
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( Request,
    Response,
    getResponseBody,
    httpLBS,
    parseRequest_,
  )
import System.Exit (die)

data Configuration = Configuration
  { serverPort :: Int,
    domain :: String,
    key :: String,
    locations :: [Location]
  }
  deriving (Show, Generic, FromJSON)

newtype Location = Location
  {location :: String}
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Environment = Environment
  { locationDataLs :: Map.Map Location [LocationData],
    connectInfo :: ConnectInfo
  }
  deriving (Show)

data ConnectInfo = ConnectInfo
  { serverPort_ :: Int,
    domain_ :: String,
    key_ :: String
  }
  deriving (Show)

mkEnvironment :: Configuration -> Environment
mkEnvironment conf = do
  let locationLs = locations conf
      ls = Map.fromList . zip locationLs $ replicate (length locationLs) []
  Environment
    { locationDataLs = ls,
      connectInfo = mkConnectInfo conf
    }

mkConnectInfo :: Configuration -> ConnectInfo
mkConnectInfo conf =
  ConnectInfo
    { serverPort_ = serverPort conf,
      domain_ = domain conf,
      key_ = key conf
    }

readConfigFile :: IO Configuration
readConfigFile = do
  content <- decodeFileEither "config.yaml"
  case content of
    Right conf -> pure conf
    Left err -> do
      print err
      die "Error reading the configuration file! Check out config.yaml!"

buildGetRequest :: Configuration -> Location -> Request
buildGetRequest conf (Location str) =
  parseRequest_ . mconcat $
    [ "https://",
      domain conf,
      "/data/2.5/weather?q=",
      str,
      "&appid=",
      key conf
    ]

responseLs :: Configuration -> IO [LC.ByteString]
responseLs conf = do
  let ls = locations conf
      resp = httpLBS . buildGetRequest conf
  mapM (fmap getResponseBody . resp) ls

data LocationData = LocationData
  { coord :: Coord,
    weather :: [Weather],
    base :: String,
    main :: Main,
    visibility :: Int,
    wind :: Wind,
    rain :: Maybe Rain,
    clouds :: Clouds,
    dt :: Int,
    sys :: Sys,
    timezone :: Int,
    id :: Int,
    name :: String,
    cod :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Coord = Coord
  { lon :: Double,
    lat :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Weather = Weather
  { idWeather :: Int, -- <--
    mainWeather :: String, -- <--
    description :: String,
    icon :: String
  }
  deriving (Show)

instance FromJSON Weather where
  parseJSON (Object v) = do
    idWeather <- v .: "id"
    mainWeather <- v .: "main"
    description <- v .: "description"
    icon <- v .: "icon"
    pure $ Weather idWeather mainWeather description icon
  parseJSON _ = mempty

instance ToJSON Weather where
  toJSON :: Weather -> Value
  toJSON (Weather id main description icon) =
    object
      [ "idWeather" .= id,
        "mainWeather" .= main,
        "description" .= description,
        "icon" .= icon
      ]

data Main = Main
  { temp :: Double,
    feels_like :: Double,
    temp_min :: Double,
    temp_max :: Double,
    pressure :: Int,
    humidity :: Int,
    sea_level :: Int,
    grnd_level :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Wind = Wind
  { speed :: Double,
    deg :: Int,
    gust :: Maybe Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype Rain = Rain
  {per1h :: Double}
  deriving (Show, Generic, ToJSON)

instance FromJSON Rain where
  parseJSON (Object v) = do
    per1h <- v .: "1h"
    pure $ Rain per1h
  parseJSON _ = mempty

newtype Clouds = Clouds
  {all :: Int}
  deriving (Show, Generic, FromJSON, ToJSON)

data Sys = Sys
  { typeSys :: Maybe Int, -- <--
    idSys :: Maybe Int, -- <--
    country :: String,
    sunrise :: Int,
    sunset :: Int
  }
  deriving (Show)

instance FromJSON Sys where
  parseJSON (Object v) = do
    typeSys <- v .:? "type"
    idSys <- v .:? "id"
    country <- v .: "country"
    sunrise <- v .: "sunrise"
    sunset <- v .: "sunset"
    pure $ Sys typeSys idSys country sunrise sunset
  parseJSON _ = mempty

instance ToJSON Sys where
  toJSON (Sys typeSys id country sunrise sunset) =
    object
      [ "typeSys" .= typeSys,
        "idSys" .= id,
        "country" .= country,
        "sunrise" .= sunrise,
        "sunset" .= sunset
      ]

checkingWorkJSON :: Configuration -> IO ()
checkingWorkJSON conf = do
  let x = httpLBS . buildGetRequest conf $ Location "Vancouver" :: IO (Response LC.ByteString)
  resp <- fmap getResponseBody x
  case eitherDecode resp :: Either String LocationData of
    Right obj -> do
      print obj
      putStrLn $ replicate 50 'X'
      print $ encode obj
    Left e -> putStrLn e
