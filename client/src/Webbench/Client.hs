{-# LANGUAGE OverloadedStrings #-}

module Webbench.Client
  (
    ClientDone(..),
    ClientConfig,
    ClientAsync,
    ClientOK,
    ClientCount
  ) where

import Control.Exception ( SomeException, handle )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Async ( async, Async )
import qualified Network.Wreq as Wreq
import Data.Pool
import Data.Time.Click ( UTCTime )
import qualified Data.Text as Text
import Data.Text.Encoding ( decodeUTF8 )
import Network.HTTP.Client (

newtype ClientOK = ClientOK ()

data ClientDone = ClientDone
  {
    clientStartTime :: UTCTime,
    clientEndTime :: UTCTime,
    clientResult :: Either SomeException Wreq.Status
  }

type ClientAsync = Async ClientDone

data ClientConfig = ClientConfig
  {
    apiCall :: String -> IO (Wreq.Response Bytestring) -- Given an endpoint path, return the Response
  }

type ClientCount = Int

setupClients :: ClientCount -> IO ClientConfig
setupClients count = do
    return ClientConfig
      {
        apiCall = call
      }
  where
    host = "localhost" --TODO Read this from an environment variable
    port = 3000 --TODO Read this from an environment variable
    prefix = "http://" ++ host ++ ":" ++ (show port)
    opts = Wreq.defaults & Wreq.manager .~ Left (
                                                  defaultManagerSettings {
                                                                         }
                                                )
    call endpoint = Wreq.getWith opts $ prefix ++ endpoint

cleanupClients :: ClientConfig -> IO ()
cleanupClients = undefined

startClient :: IO ClientAsync
startClient = undefined
