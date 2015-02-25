module Server where

import           App.Prelude
import qualified App.Time as Time

import           Data.List (intersperse)
import qualified Data.IntMap as IM
import qualified Control.Exception as E
import qualified Control.Monad.Cont as C
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Maybe

import qualified Log as Log
import           Types
import           Exception
import           GameController (spawnControlThread)
import           Concurrent



runClientThread :: Server -> Handle -> Concurrent ()
runClientThread srv@Server{..} hdl = do
    !() <- liftIO $ hSetBuffering hdl LineBuffering
    !cl <- initClient srv hdl
    groupSelectRepl srv cl
--{-# NOINLINE runClientThread #-}


groupSelectRepl :: Server -> Client -> Concurrent ()
groupSelectRepl srv@Server{..} cl = liftIO loop
  where
    loop = forever $ do
        runCIO (\a -> return a) $ do
            !() <- showGroups srv cl
            !mmsg <- getUserMessage srv cl

            !() <- case mmsg of
                Just msg -> handleClientMessage srv cl msg
                Nothing -> return ()
            return ()
--{-# NOINLINE groupSelectRepl #-}

handleClientMessage :: Server -> Client -> ClientMessage -> Concurrent ()
handleClientMessage srv cl msg = case msg of
    Quit -> throwCIO QuitGame

    NewGroup name capacity time timeout -> do
        !gr <- createGroupCIO srv name capacity time timeout
        !() <- joinAndThen srv gr cl
        return ()

    JoinGroup gid -> do
        !mgr <- atomically_ $ getGroup srv gid
        case mgr of
            Just gr -> do
                !() <- joinAndThen srv gr cl
                return ()
            Nothing -> return ()

createGroupCIO :: Server -> GroupName -> GroupCapacity -> PlayTime -> Timeout -> CIO r Group
createGroupCIO srv name cap time to = do
    !gid <- liftIO newUniqueInt
    !ts <- liftIO Time.getUnixTimeAsInt
    !gr <- atomically_ $ createGroup srv gid name cap time ts to
    !() <- tick srv Log.GroupNew
    !() <- spawnTimeoutCanceler srv gr
    return gr

joinAndThen :: Server -> Group -> Client -> Concurrent ()
joinAndThen srv gr cl = mask_ $ \restore -> do
    !joinSuccess <- addClient srv gr cl
    if joinSuccess
        then do
            let
                finalizer = removeClient srv cl gr
                clientErrorHandler = \ (_ :: ClientException) -> return ()
                action = do
                    !() <- notifyClient srv gr cl
                    !() <- runClient srv gr cl
                    return ()
            !() <- restore action
                    `catch_` clientErrorHandler
                    `finally_` finalizer
            return ()
        else
            return ()


showGroups :: Server -> Client -> Concurrent ()
showGroups srv cl = do
    !grs -- :: [(GroupId, Group)]
        <- atomically_ $ getAllGroups srv

    clientPut srv cl $ "!groups " <> (mconcat $ intersperse " " $ map (expr . fst) grs) <> "\n"
--            clientPut srv cl $ mconcat
--                [ "{\"rooms\":["
--                , mconcat $ intersperse "," $ map (expr . fst) grs
--                , "],"
--                , "\"status\":\"groupSelect\""
--                , "}"
--                , "\n"
--                ]


getUserMessage :: Server -> Client -> Concurrent (Maybe ClientMessage)
getUserMessage srv cl = do
    input :: ShortByteString
        <- clientGet srv cl
    case words input of
        ["/quit"] -> return $ Just Quit
        ["/new", name, cap', playtime', timeout'] -> return $ do
            (!capacity, _) <- readInt cap'
            (!playtime, _) <- readInt playtime'
            (!timeout, _) <- readInt timeout'
            return $ NewGroup name capacity playtime timeout
        ["/join", gid'] -> return $ do
            (!gid, _) <- readInt gid'
            return $ JoinGroup gid
        _ -> return Nothing
--{-# NOINLINE getUserMessage #-}

initClient :: Server -> Handle -> Concurrent Client
initClient srv hdl = do
    !cl <- newClient hdl
    tick srv $ Log.ClientNew
    clientPut srv cl $ "!init " <> (expr $ clientId cl) <> "\n"
--    clientPut cl $ "{\"clientId\":" <> (expr $ clientId cl) <> "}\n"
    return cl
--{-# NOINLINE initClient #-}


notifyClient :: Server -> Group -> Client -> Concurrent ()
notifyClient srv@Server{..} gr@Group{..} cl@Client{..} = do

    -- Notice group to User
    clientPut srv cl $ "!event join " <> expr groupId <> "\n"
--{-# NOINLINE notifyClient #-}


runClient :: Server -> Group -> Client -> Concurrent ()
runClient srv@Server{..} gr@Group{..} cl@Client{..} = do

    !broadcastCh <- atomically_ $ dupTChan groupBroadcastChan

    -- Spawn 3 linked threads.
    !() <- race_
            (broadcastReceiver cl broadcastCh)
            (race_ (clientReceiver srv cl) (clientServer srv gr cl))
    return ()
--{-# NOINLINE runClient #-}


broadcastReceiver :: Client -> TChan Message -> Concurrent ()
broadcastReceiver cl broadcastCh = forever $ do
    !msg <- atomically_ $ readTChan broadcastCh
    !() <- atomically_ $ sendMessage cl msg
    return ()


clientReceiver :: Server -> Client -> Concurrent ()
clientReceiver srv cl = forever $ do
    !str <- clientGet srv cl
    !() <- atomically_ $ sendMessage cl (Command str)
    return ()
--{-# NOINLINE clientReceiver #-}


clientServer :: Server -> Group -> Client -> Concurrent ()
clientServer srv gr cl@Client{..} = do

    !msg <- atomically_ $ readTChan clientChan
    !continue <- handleMessage srv gr cl msg
    if continue
        then clientServer srv gr cl
        else return ()
    -- Left the room if continue == False.
--{-# NOINLINE clientServer #-}

addClient :: Server -> Group -> Client -> Concurrent Bool
addClient srv@Server{..} gr@Group{..} cl@Client{..} = join $ atomically_ $ do

    mCnt <- runMaybeT $ do
        !cnt <- MaybeT $ Just <$> readTVar groupMemberCount
        guard $ cnt < groupCapacity

        !gameSt <- MaybeT $ Just <$> getGameState gr
        guard $ gameSt == Waiting

        !clientMap <- MaybeT $ Just <$> readTVar groupMembers
        guard $ not (IM.member clientId clientMap)

        return cnt

    case mCnt of
        Just cnt -> do
            let
                gotFull = cnt + 1 == groupCapacity

            modifyTVar' groupMembers $ IM.insert clientId cl
            modifyTVar' groupMemberCount succ
            when gotFull
                    $ changeGameState gr BeforePlay

            return $ do -- CIO ()
                when gotFull $ do
                    -- Members are gathered.
                    -- TODO: Is Transaction required to get canceler's ThreadId?
                    killThread =<< (atomically_ $ readTMVar groupCanceler)
                    -- TODO: Exception Handling, supervisored threading.
                    spawnControlThread srv gr
                logger srv $ mconcat
                    [ "Client<" <> expr clientId <> "> joind Group<" <> expr groupId <> ">."
                    , " Room members are <" <> (expr $ cnt + 1) <> ">."
                    ]
                return True
        Nothing -> return $ return False


removeClient :: Server -> Client -> Group -> Concurrent ()
removeClient srv@Server{..} cl@Client{..} gr@Group{..} = do
    join $ atomically_ $ do
        !mcl <- getClient clientId gr
        case mcl of
            Just _ -> do
                !cnt <- readTVar groupMemberCount
                modifyTVar' groupMembers (IM.delete clientId)
                modifyTVar' groupMemberCount pred

                sendBroadcast gr (Notice $ "Client<" <> expr clientId <> "> is left.")

                mOnRemove <- if (cnt == 1)
                    then do
                        onRemove <- deleteGroupCIO srv gr
                        return $ Just onRemove
                    else return Nothing

                return $ do
                    case mOnRemove of
                        -- Kick all members
                        -- Kill timeout canceller
                        Just onRemove -> onRemove
                        Nothing -> return ()

                    !() <- clientPut srv cl $ mconcat
                        [ "!event leave"
                        , "\n"
                        ]
--                        [ "{\"event\":\"leave-room\"}"
--                        , "\n"
--                        ]
                    tick srv Log.GroupLeft
                    logger srv $ mconcat
                        [ "Client<" <> expr clientId <> "> is removed from Group<" <> expr groupId <> ">."
                        , " Room members are <" <> (expr $ cnt - 1) <> ">."
                        ]
            Nothing -> do
                return $ return ()
--{-# NOINLINE removeClient #-}

handleMessage :: Server -> Group -> Client -> Message -> Concurrent Bool
handleMessage srv@Server{..} gr@Group{..} cl@Client{..} msg = do
    -- Send message to client
    output cl msg

    case msg of
        Command str -> do
            case words str of
                ["/leave"] -> do
                    -- Leave the room.
                    return False

                ["/quit"] -> do
                    -- Quit the game.
                    throwCIO QuitGame

                [] -> do
                    -- Ignore empty messages.
                    return True

                _ -> do
                    tick srv Log.GroupChat
                    atomically_ $ sendBroadcast gr (Broadcast clientId str)
                    return True
        Broadcast _ _ -> do
            return True

        Notice _ -> do
            return True

        _ -> error "Not impl yet"
--{-# NOINLINE handleMessage #-}


spawnTimeoutCanceler :: Server -> Group -> CIO r ()
spawnTimeoutCanceler srv gr = void $ fork_ $ do
    !tid <- myThreadId
    atomically_ $ putTMVar (groupCanceler gr) tid
    threadDelay $ groupTimeout gr * 1000 * 1000
    cancelWaiting srv gr


deleteGroupCIO :: Server -> Group -> CSTM r (Concurrent ())
deleteGroupCIO srv gr = do
    onRemove <- deleteGroup srv gr
    kill <- killCanceler gr
    return $ do
        onRemove
        -- Killing the canceler thread must be done at last
        -- so that the canceler thread itself can call deleteGroupCIO.
        kill
