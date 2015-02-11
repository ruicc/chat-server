module Log where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception

import           Data.Monoid


type LogChan = TChan String

spawnLogger :: ErrorChan -> IO LogChan
spawnLogger erCh = do
    let
        supervisor ch = do
            as :: Async ()
                <- async (logger ch)
            res :: Either SomeException ()
                <- try $ wait as
            case res of
                Left e -> do
                    atomically $ writeTChan erCh e
                    supervisor ch
                Right _ -> error "ここには来ない"

        logger :: LogChan -> IO ()
        logger ch = forever $ do
            str <- atomically $ readTChan ch
            putStrLn $ "Log: " <> str

    ch :: LogChan
        <- newTChanIO

    _tid <- forkIO $ supervisor ch
    return ch

------------------------------------------------------------------------------------------

type StatChan = TChan AppEvent

data AppEvent
    = ClientNew
    | ClientLeft
    | GroupNew
    | GroupChat
    | GroupJoin
    | GroupLeft

data Summary = Summary 
    { clientNew :: Int
    , clientLeft :: Int
    , groupNew :: Int
    , groupChat :: Int
    , groupJoin :: Int
    , groupLeft :: Int
    }
    deriving Show

spawnAggregator :: ErrorChan -> IO StatChan
spawnAggregator erCh = do
    let
        zeroSummary = newTVarIO (Summary 0 0 0 0 0 0)

        supervisor ch tsum = do
            as :: Async ()
                <- async (aggregator ch tsum)
            res :: Either SomeException ()
                <- try $ wait as

            case res of
                Left e -> do
                    atomically $ writeTChan erCh e
                    supervisor ch tsum
                Right _ -> error "ここには来ない"

        aggregator :: StatChan -> TVar Summary -> IO ()
        aggregator ch tsum = do
            stat
                <- atomically $ readTChan ch
            atomically $ aggregate tsum stat
            aggregator ch tsum

        aggregate sum ClientNew  = modifyTVar' sum $ \s -> s { clientNew = succ $ clientNew s }
        aggregate sum ClientLeft = modifyTVar' sum $ \s -> s { clientLeft = succ $ clientLeft s }
        aggregate sum GroupNew  = modifyTVar' sum $ \s -> s { groupNew = succ $ groupNew s }
        aggregate sum GroupChat  = modifyTVar' sum $ \s -> s { groupChat = succ $ groupChat s }
        aggregate sum GroupJoin  = modifyTVar' sum $ \s -> s { groupJoin = succ $ groupJoin s }
        aggregate sum GroupLeft  = modifyTVar' sum $ \s -> s { groupLeft = succ $ groupLeft s }

        outputRepeatedly :: TVar Summary -> IO ()
        outputRepeatedly tsum = do
            sum <- atomically $ readTVar tsum
            print sum
            threadDelay $ 5 * 1000 * 1000
            outputRepeatedly tsum

    tsum :: TVar Summary
        <- zeroSummary

    ch :: StatChan
        <- newTChanIO

    _tid <- forkIO $ supervisor ch tsum
    _tid2 <- forkIO $ outputRepeatedly tsum
    return ch

------------------------------------------------------------------------------------------

type ErrorChan = TChan SomeException

spawnErrorCollector :: IO ErrorChan
spawnErrorCollector = do
    let
        supervisor ch = do
            as :: Async ()
                <- async (collector ch)
            res :: Either SomeException ()
                <- try $ wait as
            case res of
                Left e -> do
                    atomically $ writeTChan ch e
                    supervisor ch
                Right _ -> error "ここには来ない"

        collector :: ErrorChan -> IO ()
        collector ch = forever $ do
            err <- atomically $ readTChan ch
            putStrLn $ "Err: " <> show err

    ch :: ErrorChan
        <- newTChanIO

    _tid <- forkIO $ supervisor ch
    return ch
