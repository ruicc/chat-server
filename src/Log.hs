module Log where

import           App.Prelude
import qualified Data.ByteString as B
import           Control.Concurrent
import           Control.Exception


type LogChan = TChan ShortByteString

spawnLogger :: ErrorChan -> StatChan -> LogChan -> IO ()
spawnLogger erCh stCh logCh = do
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
            str <- atomically $ do
                writeTChan stCh Logging
                readTChan ch
            when (not $ null str) $ putStrLn $ "Log: " <> str
            return ()

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
    | Logging

data AppStat = AppStat
    { clientNew :: TVar Int
    , clientLeft :: TVar Int
    , groupNew :: TVar Int
    , groupChat :: TVar Int
    , groupJoin :: TVar Int
    , groupLeft :: TVar Int
    , systemErrors :: TVar Int
    , logging :: TVar Int
    }

printStat :: AppStat -> IO ()
printStat AppStat{..} = do
    (cn, cl, gn, gc, gj, gl, se, l) <- atomically $ do
        cn <- readTVar $ clientNew
        cl <- readTVar $ clientLeft
        gn <- readTVar $ groupNew
        gc <- readTVar $ groupChat
        gj <- readTVar $ groupJoin
        gl <- readTVar $ groupLeft
        se <- readTVar $ systemErrors
        l <-  readTVar $ logging
        return (cn, cl, gn, gc, gj, gl, se, l)
    putStrLn $ mconcat
        [ "cl new: " <> expr cn <> ", "
        , "cl left: " <> expr cl <> ", "
        , "gr new: " <> expr gn <> ", "
        , "gr chat: " <> expr gc <> ", "
        , "gr join: " <> expr gj <> ", "
        , "gr left: " <> expr gl <> ", "
        , "sys err: " <> expr se <> ", "
        , "log: " <> expr l
        ]


zeroStat :: IO AppStat
zeroStat = do
    cn <- newTVarIO 0
    cl <- newTVarIO 0
    gn <- newTVarIO 0
    gc <- newTVarIO 0
    gj <- newTVarIO 0
    gl <- newTVarIO 0
    se <- newTVarIO 0
    l <- newTVarIO 0
    return $ AppStat cn cl gn gc gj gl se l


spawnStatAggregator :: ErrorChan -> StatChan -> AppStat -> IO ()
spawnStatAggregator erCh stCh stat = do
    let

        supervisor ch astat = do
            as :: Async ()
                <- async (aggregator ch astat)
            res :: Either SomeException ()
                <- try $ wait as

            case res of
                Left e -> do
                    atomically $ writeTChan erCh e
                    supervisor ch astat
                Right _ -> error "ここには来ない"

        aggregator :: StatChan -> AppStat -> IO ()
        aggregator ch tsum = do
            st <- atomically $ readTChan ch
            atomically $ aggregate tsum st
            aggregator ch tsum

        aggregate astat ClientNew   = modifyTVar' (clientNew    astat) succ
        aggregate astat ClientLeft  = modifyTVar' (clientLeft   astat) succ
        aggregate astat GroupNew    = modifyTVar' (groupNew     astat) succ
        aggregate astat GroupChat   = modifyTVar' (groupChat    astat) succ
        aggregate astat GroupJoin   = modifyTVar' (groupJoin    astat) succ
        aggregate astat GroupLeft   = modifyTVar' (groupLeft    astat) succ
        aggregate astat SystemError = modifyTVar' (systemErrors astat) succ
        aggregate astat Logging     = modifyTVar' (logging      astat) succ

    _tid <- forkIO $ supervisor stCh stat
    return ()

------------------------------------------------------------------------------------------

type ErrorChan = TChan SomeException

spawnErrorCollector :: ErrorChan -> StatChan -> AppStat -> IO ()
spawnErrorCollector erCh stCh stat = do
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
            err <- atomically $ readTChan ch
            putStrLn $ "Err: " <> expr err
            atomically $ writeTChan stCh SystemError

    _tid <- forkIO $ supervisor erCh stat
    return ()

------------------------------------------------------------------------------------------

spawnCollectorThreads :: IO (ErrorChan, StatChan, LogChan)
spawnCollectorThreads = do
    let
        outputRepeatedly :: AppStat -> IO ()
        outputRepeatedly stat = do
            printStat stat
            threadDelay $ 5 * 1000 * 1000
            outputRepeatedly stat

    stat <- zeroStat

    erCh :: ErrorChan
        <- newTChanIO
    stCh :: StatChan
        <- newTChanIO
    logCh :: LogChan
        <- newTChanIO

    spawnErrorCollector erCh stCh stat
    spawnStatAggregator erCh stCh stat
    spawnLogger erCh stCh logCh

    forkIO $ dummyLogSender logCh

    _tid2 <- forkIO $ outputRepeatedly stat
    return (erCh, stCh, logCh)

dummyLogSender logCh = do
    threadDelay $ 5 * 1000 * 1000
    atomically $ writeTChan logCh ""
    dummyLogSender logCh

