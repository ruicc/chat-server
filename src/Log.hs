module Log where

import           App.Prelude
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception

import           Data.Monoid


type LogChan = TChan ShortByteString

spawnLogger :: ErrorChan -> LogChan -> IO ()
spawnLogger erCh logCh = do
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

    _tid <- forkIO $ supervisor logCh
    return ()

------------------------------------------------------------------------------------------

type StatChan = TChan AppEvent

data AppEvent
    = ClientNew
    | ClientLeft
    | GroupNew
    | GroupChat
    | GroupJoin
    | GroupLeft
    | SystemError

data Summary = Summary 
    { clientNew :: Int
    , clientLeft :: Int
    , groupNew :: Int
    , groupChat :: Int
    , groupJoin :: Int
    , groupLeft :: Int
    , systemErrors :: Int
    }
    deriving Show

zeroSummary :: IO (TVar Summary)
zeroSummary = newTVarIO (Summary 0 0 0 0 0 0 0)

spawnStatAggregator :: ErrorChan -> StatChan -> TVar Summary -> IO ()
spawnStatAggregator erCh stCh tsum = do
    let

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
        aggregate sum SystemError = modifyTVar' sum $ \s -> s { systemErrors = succ $ systemErrors s }

    _tid <- forkIO $ supervisor stCh tsum
    return ()

------------------------------------------------------------------------------------------

type ErrorChan = TChan SomeException

spawnErrorCollector :: ErrorChan -> StatChan -> TVar Summary -> IO ()
spawnErrorCollector erCh stCh tsum = do
    let
        supervisor ch tsum = do
            as :: Async ()
                <- async (collector ch)
            res :: Either SomeException ()
                <- try $ wait as
            case res of
                Left e -> do
                    atomically $ writeTChan ch e
                    supervisor ch tsum
                Right _ -> error "ここには来ない"

        collector :: ErrorChan -> IO ()
        collector ch = forever $ do
            err <- atomically $ do
                writeTChan stCh SystemError
                readTChan ch
            putStrLn $ "Err: " <> expr err

    _tid <- forkIO $ supervisor erCh tsum
    return ()

------------------------------------------------------------------------------------------

spawnCollectorThreads :: IO (ErrorChan, StatChan, LogChan)
spawnCollectorThreads = do
    let
        outputRepeatedly :: TVar Summary -> IO ()
        outputRepeatedly tsum = do
            sum <- atomically $ readTVar tsum
            print sum
            threadDelay $ 5 * 1000 * 1000
            outputRepeatedly tsum

    sum <- zeroSummary

    erCh :: ErrorChan
        <- newTChanIO
    stCh :: StatChan
        <- newTChanIO
    logCh :: LogChan
        <- newTChanIO

    spawnErrorCollector erCh stCh sum
    spawnStatAggregator erCh stCh sum
    spawnLogger erCh logCh


    _tid2 <- forkIO $ outputRepeatedly sum
    return (erCh, stCh, logCh)
