{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module LocationData where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Object),
    object,
    (.:),
    (.:?),
    (.=),
  )
import GHC.Generics (Generic)

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
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

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

data RequestedLocation = RequestedLocation
  { nameRL :: String,
    coordRL :: Coord,
    countryRL :: String,
    stateRL :: String
  }
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON RequestedLocation where
  parseJSON (Object v) = do
    nameRL <- v .: "name"
    lon_ <- v .: "lon"
    lat_ <- v .: "lat"
    let coordRL = Coord {lon = lon_, lat = lat_}
    countryRL <- v .: "country"
    stateRL <- v .: "state"
    pure $ RequestedLocation nameRL coordRL countryRL stateRL
  parseJSON _ = mempty
