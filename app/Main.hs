{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (eitherDecode)
-- import qualified Data.ByteString.Lazy.Char8 as LC
import Lib
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
  )
-- import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import Servant

--import qualified Data.Text as T

type API = Capture "val" String :> Get '[JSON] (Either String LocationData) -- change to Maybe

handler :: Configuration -> String -> Handler (Either String LocationData) -- change to Maybe
handler conf loc =
  eitherDecode . getResponseBody <$> httpLBS (buildGetRequest conf $ Location loc) -- change to decode

api :: Proxy API
api = Proxy

server :: Configuration -> Server API
server = handler

app :: Configuration -> Application
app conf = serve api $ server conf

main :: IO ()
main = do
  conf <- readConfigFile
  let port = serverPort conf
  putStrLn "Server is started."
  run port $ app conf

-- responseLs conf >>= print
-- print conf
