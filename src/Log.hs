module Log where

import           App.Prelude
import qualified Data.ByteString as B
--import           Control.Concurrent
--import           Control.Concurrent.STM
--import           Control.Exception

import           Concurrent


type LogChan = TChan ShortByteString

spawnLogger :: ErrorChan -> StatChan -> LogChan -> CIO r ()
spawnLogger erCh stCh logCh = do
    let
        supervisor ch = do
            as :: Async ()
                <- async (logger ch)
            res :: Either SomeException ()
                <- try $ wait as
            case res of
                Left e -> do
                    atomically_ $ writeTChan erCh e
                    supervisor ch
                Right _ -> error "ここには来ない"

        logger :: LogChan -> CIO r ()
        logger ch = forever $ do
            str <- atomically_ $ do
                writeTChan stCh Logging
                readTChan ch
            when (not $ null str) (liftIO $ putStrLn $ "Log: " <> str)
            return ()

    _tid <- fork_ $ supervisor logCh
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

printStat :: AppStat -> CIO r ()
printStat AppStat{..} = do
    (cn, cl, gn, gc, gj, gl, se, l) <- atomically_ $ do
        cn <- readTVar $ clientNew
        cl <- readTVar $ clientLeft
        gn <- readTVar $ groupNew
        gc <- readTVar $ groupChat
        gj <- readTVar $ groupJoin
        gl <- readTVar $ groupLeft
        se <- readTVar $ systemErrors
        l <-  readTVar $ logging
        return (cn, cl, gn, gc, gj, gl, se, l)
    liftIO $ putStrLn $ mconcat
        [ "cl new: " <> expr cn <> ", "
        , "cl left: " <> expr cl <> ", "
        , "gr new: " <> expr gn <> ", "
        , "gr chat: " <> expr gc <> ", "
        , "gr join: " <> expr gj <> ", "
        , "gr left: " <> expr gl <> ", "
        , "sys err: " <> expr se <> ", "
        , "log: " <> expr l
        ]


zeroStat :: CIO r AppStat
zeroStat = do
    cn <- newTVarCIO 0
    cl <- newTVarCIO 0
    gn <- newTVarCIO 0
    gc <- newTVarCIO 0
    gj <- newTVarCIO 0
    gl <- newTVarCIO 0
    se <- newTVarCIO 0
    l <- newTVarCIO 0
    return $ AppStat cn cl gn gc gj gl se l


spawnStatAggregator :: ErrorChan -> StatChan -> AppStat -> CIO r ()
spawnStatAggregator erCh stCh stat = do
    let

        supervisor :: StatChan -> AppStat -> CIO r ()
        supervisor ch astat = do
            as :: Async ()
                <- async (aggregator ch astat)
            res :: Either SomeException ()
                <- try $ wait as

            case res of
                Left e -> do
                    atomically_ $ writeTChan erCh e
                    supervisor ch astat
                Right _ -> error "ここには来ない"

        aggregator :: StatChan -> AppStat -> CIO r ()
        aggregator ch tsum = do
            st <- atomically_ $ readTChan ch
            atomically_ $ aggregate tsum st
            aggregator ch tsum

        aggregate astat ClientNew   = modifyTVar' (clientNew    astat) succ
        aggregate astat ClientLeft  = modifyTVar' (clientLeft   astat) succ
        aggregate astat GroupNew    = modifyTVar' (groupNew     astat) succ
        aggregate astat GroupChat   = modifyTVar' (groupChat    astat) succ
        aggregate astat GroupJoin   = modifyTVar' (groupJoin    astat) succ
        aggregate astat GroupLeft   = modifyTVar' (groupLeft    astat) succ
        aggregate astat SystemError = modifyTVar' (systemErrors astat) succ
        aggregate astat Logging     = modifyTVar' (logging      astat) succ

    _tid <- fork_ $ supervisor stCh stat
    return ()

------------------------------------------------------------------------------------------

type ErrorChan = TChan SomeException

spawnErrorCollector :: ErrorChan -> StatChan -> AppStat -> CIO r ()
spawnErrorCollector erCh stCh stat = do
    let
        supervisor ch tsum = do
            as :: Async ()
                <- async (collector ch)
            res :: Either SomeException ()
                <- try $ wait as
            case res of
                Left e -> do
                    atomically_ $ writeTChan ch e
                    supervisor ch tsum
                Right _ -> error "ここには来ない"

        collector :: ErrorChan -> CIO r ()
        collector ch = forever $ do
            err <- atomically_ $ readTChan ch
            liftIO $ putStrLn $ "Err: " <> expr err
            atomically_ $ writeTChan stCh SystemError

    _tid <- fork_ $ supervisor erCh stat
    return ()

------------------------------------------------------------------------------------------

spawnCollectorThreads :: IO (ErrorChan, StatChan, LogChan)
spawnCollectorThreads = runCIO return $ do
    let
        outputRepeatedly :: AppStat -> CIO r ()
        outputRepeatedly stat = do
            printStat stat
            threadDelay $ 5 * 1000 * 1000
            outputRepeatedly stat

    stat <- zeroStat

    erCh :: ErrorChan
        <- newTChanCIO
    stCh :: StatChan
        <- newTChanCIO
    logCh :: LogChan
        <- newTChanCIO

    spawnErrorCollector erCh stCh stat
    spawnStatAggregator erCh stCh stat
    spawnLogger erCh stCh logCh

    fork_ $ dummyLogSender logCh

    _tid2 <- fork_ $ outputRepeatedly stat
    return (erCh, stCh, logCh)

dummyLogSender logCh = do
    threadDelay $ 5 * 1000 * 1000
    atomically_ $ writeTChan logCh ""
    dummyLogSender logCh

