{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- import qualified Data.ByteString.Lazy.Char8 as LC
-- import qualified Network.Wai as W
import Config
import Control.Concurrent (forkIO, newMVar)
import Control.Monad.IO.Class (liftIO)
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

type API =
  Capture "location" String
    :> QueryParam "timestamp" Int
    :> Get '[JSON] (Either String LocationData) -- change to Maybe

handler :: ConnectInfo -> String -> Maybe Int -> Handler (Either String LocationData) -- change to Maybe
handler connInf loc maybeInt = do
  _ <- liftIO $ print (loc, maybeInt)
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
  mVar <- newMVar $ mkEnvironment conf
  _ <- forkIO $ do
    putStrLn "Caching is started."
    cachingLoop mVar
  let port = serverPort conf
      connInf = mkConnectInfo conf
  putStrLn "Server is started."
  run port $ app connInf

-- responseLs conf >>= print
-- print . locationDataLs . mkEnvironment $ conf
-- print conf
-- print $ mkConnectInfo conf
