{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- import qualified Data.ByteString.Lazy.Char8 as LC

-- import qualified Network.Wai as W

import Config
import Data.Aeson (eitherDecode)
import Lib
import LocationData
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
  )
import Network.Wai.Handler.Warp (run)
import Servant

--import qualified Data.Text as T

type API = Capture "val" String :> Get '[JSON] (Either String LocationData) -- change to Maybe

handler :: ConnectInfo -> String -> Handler (Either String LocationData) -- change to Maybe
handler connInf loc =
  eitherDecode . getResponseBody <$> httpLBS (buildGetRequest connInf $ Location loc) -- change to decode

api :: Proxy API
api = Proxy

server :: ConnectInfo -> Server API
server = handler

app :: ConnectInfo -> Application
app = serve api . server 

main :: IO ()
main = do
  conf <- readConfigFile
  let port = serverPort conf
      connInf = mkConnectInfo conf
  putStrLn "Server is started."
  run port $ app connInf

-- responseLs conf >>= print
-- print conf
