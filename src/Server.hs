module Server (runClientThread) where

import           App.Prelude
import qualified App.Time as Time

import           Data.List (intersperse)
import           Data.Unique as Uniq
import qualified Data.IntMap as IM

import qualified Log as Log
import           Types
import           Exception
import           GameController (spawnControlThread)
import           Concurrent



runClientThread :: Server -> Handle -> IO ()
runClientThread srv@Server{..} hdl = runConcurrent (const $ return ()) $ do
    let
        showGroups :: Server -> Client -> Concurrent ()
        showGroups srv cl = do
            grs :: [(GroupId, Group)]
                <- runSTM $ getAllGroups srv

            clientPut cl $ "!groups " <> (mconcat $ intersperse " " $ map (expr . fst) grs) <> "\n"
--            clientPut cl $ mconcat
--                [ "{\"rooms\":["
--                , mconcat $ intersperse "," $ map (expr . fst) grs
--                , "],"
--                , "\"status\":\"groupSelect\""
--                , "}"
--                , "\n"
--                ]

        getUserInput :: Client -> Concurrent ShortByteString
        getUserInput cl = do
            input :: ShortByteString
                <- clientGet srv cl

--            logger $ "Group select: " <> expr input
            return input

        loop :: Client -> Concurrent ()
        loop cl = do

            showGroups srv cl
            input :: ShortByteString
                <- getUserInput cl

            case words input of
                ["/quit"] -> throwC QuitGame

                ["/new", name, capacity', timeout'] -> do
                    let
                        playTime = 3 -- TODO: User should be able to specify it.

                    case (readInt capacity', readInt timeout') of
                        (Just (capacity, _), Just (timeout, _)) -> do
                            gid <- Uniq.hashUnique <$> liftIO Uniq.newUnique
                            ts <- liftIO Time.getUnixTimeAsInt

                            mask $ \restore -> do
                                mNewGroup :: Maybe (Group, Concurrent' () ())
                                    <- getGroupAndJoin
                                        cl
                                        (Just <$> createGroup srv gid name capacity playTime ts timeout)
                                case mNewGroup of
                                    Just (gr, onJoin) -> do
                                        -- FIXME: spawnTimeoutCanceler may throw exception..
                                        restore (spawnTimeoutCanceler srv gr >> notifyClient srv gr cl onJoin)
                                                -- Catch any exception defined by ClientException.
                                                `catch` (\ (_ :: ClientException) -> return ())
                                                -- Clean up
                                                `finally` (removeClient srv cl gr)
                                    Nothing -> return () -- TODO: エラー理由
                            loop cl

                        _   -> do
                            loop cl

                ["/join", gid'] -> do

                    case readInt gid' of
                        Just (gid, _) -> do
                            mask $ \restore -> do
                                mNewGroup :: Maybe (Group, Concurrent' () ())
                                    <- getGroupAndJoin
                                        cl
                                        (getGroup srv gid)
                                case mNewGroup of
                                    Just (gr, onJoin) -> do
                                        restore (notifyClient srv gr cl onJoin)
                                                -- Catch any exception defined by ClientException.
                                                `catch` (\ (_ :: ClientException) -> return ())
                                                -- Clean up
                                                `finally` (removeClient srv cl gr)
                                    Nothing -> return () -- TODO: エラー理由
                            loop cl

                        Nothing -> do
                            loop cl
                _ -> do
                    clientPut cl $ "!status \"group-select\"\n"
                    loop cl

        -- This signature might be a bit scary, but it just combines 2 actions,
        -- getGr and addClient.
        getGroupAndJoin :: Client -> STM (Maybe Group) -> Concurrent (Maybe (Group, Concurrent' () ()))
        getGroupAndJoin cl getGr = liftIO $ join $ atomically $ do

            -- NOTICE: Getting a group might fail due to removing group.
            mgr :: Maybe Group <- getGr
            case mgr of
                Just gr -> do
                    -- NOTICE: Joining a group might fail due to room capacity.
                    mOnJoin <- addClient srv cl gr
                    case mOnJoin of
                        -- OK, User joined.
                        Just onJoin -> return $ return $ Just (gr, onJoin)
                        -- Room capacity is full.
                        Nothing -> return $ return $ Nothing

                Nothing -> do
                    -- Getting group failed..
                    return $ return $ Nothing

    liftIO $ hSetBuffering hdl LineBuffering
    cl <- initClient srv hdl
    loop cl


initClient :: Server -> Handle -> Concurrent Client
initClient srv hdl = do
    cl <- newClient hdl
    liftIO $ tick srv $ Log.ClientNew
    clientPut cl $ "!init " <> (expr $ clientId cl) <> "\n"
--    clientPut cl $ "{\"clientId\":" <> (expr $ clientId cl) <> "}\n"
    return cl


notifyClient :: Server -> Group -> Client -> Concurrent () -> Concurrent ()
notifyClient srv@Server{..} gr@Group{..} cl@Client{..} onJoin = do

    -- Notice group to User
    clientPut cl $ "!event join " <> expr groupId <> "\n"
--    clientPut cl $ mconcat
--        [ "{\"event\":\"join-room\"}"
--        , "\n"
--        ]

    onJoin

    runClient srv gr cl

runClient :: Server -> Group -> Client -> Concurrent ()
runClient srv@Server{..} gr@Group{..} cl@Client{..} = do
    let
        broadcastReceiver :: TChan Message -> Concurrent ()
        broadcastReceiver broadcastCh = forever $ do
            runSTM $ do
                msg :: Message
                    <- readTChan broadcastCh
                sendMessage cl msg
--            logger $ "BroadcastReceiver works"

        receiver :: Concurrent' () ()
        receiver = forever $ do
            str <- clientGet srv cl

--            logger $ "Client<" <> (expr clientId) <> "> entered raw strings: " <> expr str
            runSTM $ sendMessage cl (Command str)

        server :: Concurrent' () ()
        server = do

            msg :: Message
                <- runSTM $ readTChan clientChan
            continue <- handleMessage srv gr cl msg
            when continue server
            -- Left the room if continue == False.

        race_' :: Concurrent' () () -> Concurrent' () () -> Concurrent' () ()
        race_' a b = liftIO $ race_ (runConcurrent return a) (runConcurrent return b)

    broadcastCh <- runSTM $ dupTChan groupBroadcastChan

    -- Spawn 3 linked threads.
    race_' (broadcastReceiver broadcastCh) (race_' receiver server)


-- | Add client to group. Returned action is to show latest history.
addClient :: Server -> Client -> Group -> STM (Maybe (Concurrent' () ()))
addClient srv@Server{..} cl@Client{..} gr@Group{..} = do
    clientMap <- readTVar groupMembers
    cnt <- readTVar groupMemberCount
    gameSt <- getGameState gr

    if IM.member clientId clientMap
        -- User has already joined.
        then return Nothing

        else if cnt >= groupCapacity || gameSt == GroupDeleted
            -- Room is full.
            then return Nothing

            else do -- STM
                writeTVar groupMembers $ IM.insert clientId cl clientMap
                modifyTVar' groupMemberCount succ

                -- To next state
                when (cnt + 1 == groupCapacity) $ changeGameState gr BeforePlay

                -- TODO: Use it later
                hist :: [Message]
                    <- getHistory gr

                sendBroadcast gr (Notice $ "Client<" <> expr clientId <> "> is joined.")

                return $ Just $ liftIO $ do -- IO

                    when (cnt + 1 == groupCapacity) $ do
                        -- Members are gathered.
                        -- TODO: Is Transaction required to get canceler's ThreadId?
                        killThread =<< (runSTM $ readTMVar groupCanceler)
                        -- TODO: Exception Handling, supervisored threading.
                        spawnControlThread srv gr

                    -- Show history
                    forM_ (reverse hist) $ \msg -> output cl msg
                    tick Log.GroupJoin
--                    logger $ mconcat
--                        [ "Client<" <> expr clientId <> "> is added to Group<" <> expr groupId <> ">."
--                        , " Room members are <" <> expr (cnt + 1) <> ">."
--                        ]


removeClient :: Server -> Client -> Group -> Concurrent ()
removeClient srv@Server{..} cl@Client{..} gr@Group{..} = liftIO $ join $ atomically $ do
    cnt <- readTVar groupMemberCount
    mcl :: Maybe Client
        <- getClient clientId gr
    case mcl of
        Just _ -> do
            modifyTVar' groupMembers (IM.delete clientId)
            modifyTVar' groupMemberCount pred

            sendBroadcast gr (Notice $ "Client<" <> expr clientId <> "> is left.")

            mOnRemove <- if (cnt == 1)
                then do
                    onRemove <- deleteGroup srv gr
                    return $ Just $ do
                        onRemove
                else return Nothing

            return $ liftIO $ do
                case mOnRemove of
                    -- Kill timeout canceller
                    Just onRemove -> onRemove
                    Nothing -> return ()

                clientPut cl $ mconcat
                    [ "!event leave"
                    , "\n"
                    ]
--                    [ "{\"event\":\"leave-room\"}"
--                    , "\n"
--                    ]
                tick Log.GroupLeft
--                logger $ mconcat
--                    [ "Client<" <> expr clientId <> "> is removed from Group<" <> expr groupId <> ">."
--                    , " Room members are <" <> (expr $ cnt - 1) <> ">."
--                    ]
        Nothing -> do
            return $ do
                logger $ mconcat
                    [ "Client<" <> expr clientId <> "> doesn't exist in Group<" <> expr groupId <> ">."
                    , " Room members are <" <> expr cnt <> ">."
                    ]

handleMessage :: Server -> Group -> Client -> Message -> Concurrent Bool
handleMessage Server{..} gr@Group{..} cl@Client{..} msg = do
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
                    throwC QuitGame

                [] -> do
                    -- Ignore empty messages.
                    return True

                _ -> do
                    tick Log.GroupChat
                    atomically $ sendBroadcast gr (Broadcast clientId str)
                    return True
        Broadcast _ _ -> do
            return True

        Notice _ -> do
            return True

        _ -> error "Not impl yet"


spawnTimeoutCanceler :: Server -> Group -> Concurrent ()
spawnTimeoutCanceler srv gr = void $ forkC $ do
    tid <- myThreadId
    atomically $ putTMVar (groupCanceler gr) tid
    threadDelay $ groupTimeout gr * 1000 * 1000
    cancelWaiting srv gr
