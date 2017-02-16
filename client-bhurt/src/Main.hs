module Main where

import System.Environment ( getArgs )
import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.Async ( async, Async, waitAny, waitEither, uninterruptibleCancel )
import Webbench.Client ( startClient, ClientDone )
import Webbench.Reporter ( startReporter, handleClientDone, stopReporter )
import Data.Time.Units ( TimeUnit, Minute, Second, toMicroseconds )
import Data.List ( delete )

newtype TimeoutReached = TimeoutReached () deriving (Read,Show,Ord,Eq)
type TimerAsync = Async TimeoutReached
type ClientAsync = Async ClientDone
type ClientResult = Either TimeoutReached ClientDone
type ResultAsync = Async ClientResult
type ClientCount = Int
type ResultHandler = ClientResult -> IO ()

main :: IO ()
main = do
  (arg1 : arg2 : _) <- getArgs
  let clientCount = read arg1
  let timeout = (read arg2)::Second
  runWarmUp clientCount
  runTest clientCount timeout

runTest :: TimeUnit t => ClientCount -> t -> IO ()
runTest count timeout = do
    reporter <- startReporter
    timer <- createTimeoutAsync timout
    clientList <- createClientList clientCount timer
    reporterStopper <- async $ do
      _ <- wait timer
      threadDelay $ toMicroseconds oneSecond -- Give the reporter a chance to finish consuming things
      stopReporter reporter
    runClients clientList (handleDone reporter)
    wait reporterStopper
  where
    oneSecond :: Second
    oneSecond = fromIntegral 1
    handleDone reporter (Left  _) = return ()
    handleDone reporter (Right x) = handleClientDone reporter x

runWarmUp :: ClientCount -> IO ()
runWarmUp clientCount = do
    timer <- createTimeoutAsync timeout
    clientList <- createClientList clientCount timer
    runClients clientList (\_ -> return ())
  where
    timeout :: Minute
    timeout = fromIntegral 1

runClients :: [IO ResultAsync] -> ResultHandler -> IO ()
runClients clients handler = doRun clients []
  where
    doRun :: [IO ResultAsync] -> [ResultAsync] -> IO ()
    doRun [] allRunning = handleResults allRunning handler
    doRun (x:xs) allRunning = do
      running <- x
      doRun xs (running:allRunning)

handleResults :: [ResultAsync] -> ResultHandler -> IO ()
handleResults []     _       = return ()
handleResults asyncs handler = do
  (async, result) <- waitAny async
  handler result
  let newAsyncs = delete async asyncs
  handlResults newAsyncs handler

timeoutClient :: TimerAsync -> IO ClientAsync -> IO ResultAsync
timeoutClient timerAsync clientIo =
    clientIo >>= \clientAsync -> do
      result <- waitEither timer clientAsync
      cancelIfLeft result clientAsync
      return result
  where
    cancelIfLeft (Left _) clientAsync = uninterruptibleCancel clientAsync
    cancelIfLeft _        _           = return ()

createClientList :: ClientCount -> TimerAsync -> [IO ResultAsync]
createClientList count timer =
    timeoutList
  where
    timeoutList :: [IO ResultAsync]
    timeoutList = map timeout asyncList
    timeout = timeoutClient timer
    asyncList :: [IO ClientAsync]
    asyncList = map async basicList
    basicList :: [IO ClientDone]
    basicList = replicate count startClient

createTimeoutAsync :: TimeUnit t => t -> IO TimerAsync
createTimeoutAsync timeout = async $ do
    threadDelay $ toMicroseconds timeout
    return $ TimeoutReached ()
