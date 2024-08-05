module Main where

import qualified Data.ByteString.Lazy.Char8 as LC
import Lib
import Network.HTTP.Simple
  ( Request,
    getResponseBody,
    httpLBS,
    parseRequest_,
  )

buildGetRequest :: Configuration -> Location -> Request
buildGetRequest conf (Location lat lon) =
  parseRequest_ . mconcat $
    [ "https://",
      domain conf,
      "/data/2.5/weather?lat=",
      show lat,
      "&lon=",
      show lon,
      "&appid=",
      key conf
    ]

-- ans = runClientM
responseLs :: Configuration -> IO [LC.ByteString]
responseLs conf = do
  let ls = locations conf
      resp = httpLBS . buildGetRequest conf
  mapM (fmap getResponseBody . resp) ls

main :: IO ()
main = do
  conf <- readConfigFile
  responseLs conf >>= print

--  print conf
