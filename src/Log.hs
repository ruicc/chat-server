module Log where

import           App.Prelude


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
--            putStrLn $ "Log: " <> str
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

data AppStat = AppStat
    { clientNew :: Int
    , clientLeft :: Int
    , groupNew :: Int
    , groupChat :: Int
    , groupJoin :: Int
    , groupLeft :: Int
    , systemErrors :: Int
    }
    deriving Show

zeroStat :: IO (TVar AppStat)
zeroStat = newTVarIO (AppStat 0 0 0 0 0 0 0)

spawnStatAggregator :: ErrorChan -> StatChan -> TVar AppStat -> IO ()
spawnStatAggregator erCh stCh stat = do
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

        aggregator :: StatChan -> TVar AppStat -> IO ()
        aggregator ch tsum = do
            st
                <- atomically $ readTChan ch
            atomically $ aggregate tsum st
            aggregator ch tsum

        aggregate tsum ClientNew  = modifyTVar' tsum $ \s -> s { clientNew = succ $ clientNew s }
        aggregate tsum ClientLeft = modifyTVar' tsum $ \s -> s { clientLeft = succ $ clientLeft s }
        aggregate tsum GroupNew  = modifyTVar' tsum $ \s -> s { groupNew = succ $ groupNew s }
        aggregate tsum GroupChat  = modifyTVar' tsum $ \s -> s { groupChat = succ $ groupChat s }
        aggregate tsum GroupJoin  = modifyTVar' tsum $ \s -> s { groupJoin = succ $ groupJoin s }
        aggregate tsum GroupLeft  = modifyTVar' tsum $ \s -> s { groupLeft = succ $ groupLeft s }
        aggregate tsum SystemError = modifyTVar' tsum $ \s -> s { systemErrors = succ $ systemErrors s }

    _tid <- forkIO $ supervisor stCh stat
    return ()

------------------------------------------------------------------------------------------

type ErrorChan = TChan SomeException

spawnErrorCollector :: ErrorChan -> StatChan -> TVar AppStat -> IO ()
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
            err <- atomically $ do
                writeTChan stCh SystemError
                readTChan ch
            putStrLn $ "Err: " <> expr err

    _tid <- forkIO $ supervisor erCh stat
    return ()

------------------------------------------------------------------------------------------

spawnCollectorThreads :: IO (ErrorChan, StatChan, LogChan)
spawnCollectorThreads = do
    let
        outputRepeatedly :: TVar AppStat -> IO ()
        outputRepeatedly tsum = do
            s <- atomically $ readTVar tsum
            print s
            threadDelay $ 5 * 1000 * 1000
            outputRepeatedly tsum

    stat <- zeroStat

    erCh :: ErrorChan
        <- newTChanIO
    stCh :: StatChan
        <- newTChanIO
    logCh :: LogChan
        <- newTChanIO

    spawnErrorCollector erCh stCh stat
    spawnStatAggregator erCh stCh stat
    spawnLogger erCh logCh


    _tid2 <- forkIO $ outputRepeatedly stat
    return (erCh, stCh, logCh)
