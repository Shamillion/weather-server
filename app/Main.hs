{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as LC
import Lib
import Network.HTTP.Simple
  ( Request,
    getResponseBody,
    httpLBS,
    parseRequest_,
  )
--import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import Servant
--import qualified Data.Text as T

type LocationAPI =
  Capture "" String :> Get '[JSON] String

type API = LocationAPI

api :: Proxy API
api = Proxy

server :: Configuration -> Server LocationAPI
server conf loc = do
  let loc' = Location loc
  LC.unpack . getResponseBody <$> httpLBS (buildGetRequest conf loc')

app :: Configuration -> Application
app conf = serve api $ server conf

buildGetRequest :: Configuration -> Location -> Request
buildGetRequest conf (Location name) =
  parseRequest_ . mconcat $
    [ "https://",
      domain conf,
      "/data/2.5/weather?q=",
      name,
      "&appid=",
      key conf
    ]

responseLs :: Configuration -> IO [LC.ByteString]
responseLs conf = do
  let ls = locations conf
      resp = httpLBS . buildGetRequest conf
  mapM (fmap getResponseBody . resp) ls

main :: IO ()
main = do
  conf <- readConfigFile
  let port = serverPort conf
  run port $ app conf

-- responseLs conf >>= print
-- print conf
