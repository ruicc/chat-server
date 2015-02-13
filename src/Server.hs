module Server (runClientThread) where

import           App.Prelude
import qualified App.Time as Time

import           Data.List (intersperse)
import           Data.Unique as Uniq
import qualified Data.IntMap as IM

import qualified Log as Log
import           Types
import           Exception



runClientThread :: Server -> Handle -> IO ()
runClientThread srv@Server{..} hdl = do
    let
        showGroups :: Server -> Client -> IO ()
        showGroups srv cl = do
            grs :: [(GroupId, Group)]
                <- atomically $ getAllGroups srv

            clientPut cl $ "/groups " <> (mconcat $ intersperse " " $ map (expr . fst) grs) <> "\n"
--            clientPut cl $ mconcat
--                [ "{\"rooms\":["
--                , mconcat $ intersperse "," $ map (expr . fst) grs
--                , "],"
--                , "\"status\":\"groupSelect\""
--                , "}"
--                , "\n"
--                ]

        getUserInput :: Client -> IO ShortByteString
        getUserInput cl = do
            input :: ShortByteString
                <- clientGet srv cl

--            logger $ "Group select: " <> expr input
            return input

        loop :: Client -> IO ()
        loop cl = do

            showGroups srv cl
            input :: ShortByteString
                <- getUserInput cl

            case words input of
                ["/quit"] -> throwIO QuitGame

                ["/new", name, capacity', timeout'] -> do
                    case (readInt capacity', readInt timeout') of
                        (Just (capacity, _), Just (timeout, _)) -> do
                            gid <- Uniq.hashUnique <$> Uniq.newUnique
                            ts <- Time.getUnixTimeAsInt

                            mask $ \restore -> do
                                mNewGroup :: Maybe (Group, IO ())
                                    <- getGroupAndJoin
                                        cl
                                        (Just <$> createGroup srv gid name (GroupCapacity capacity) ts timeout)
                                case mNewGroup of
                                    Just (gr, onJoin) -> do

                                        -- Timeout
                                        -- TODO: This is on mask. Is it OK??
                                        void $ forkIO $ do
                                            threadDelay $ groupTimeout gr * 1000 * 1000
                                            (groupCancelWaiting gr) srv gr

                                        restore (notifyClient srv gr cl onJoin)
                                                -- Catch any exception defined by ClientException.
                                                `catch` (\ (_ :: ClientException) -> return ())
                                                -- Clean up
                                                `finally` (join $ atomically $ removeClient srv cl gr)
                                    Nothing -> return () -- TODO: エラー理由
                            loop cl

                        _   -> do
                            loop cl

                ["/join", gid'] -> do

                    case readInt gid' of
                        Just (gid, _) -> do
                            mask $ \restore -> do
                                mNewGroup :: Maybe (Group, IO ())
                                    <- getGroupAndJoin
                                        cl
                                        (getGroup srv gid)
                                case mNewGroup of
                                    Just (gr, onJoin) -> do
                                        restore (notifyClient srv gr cl onJoin)
                                                -- Catch any exception defined by ClientException.
                                                `catch` (\ (_ :: ClientException) -> return ())
                                                -- Clean up
                                                `finally` (join $ atomically $ removeClient srv cl gr)
                                    Nothing -> return () -- TODO: エラー理由
                            loop cl

                        Nothing -> do
                            loop cl
                _ -> do
                    loop cl

        -- This signature might be a bit scary, but it just combines 2 processes,
        -- getGr and addClient.
        getGroupAndJoin :: Client -> STM (Maybe Group) -> IO (Maybe (Group, IO ()))
        getGroupAndJoin cl getGr = join $ atomically $ do

            -- NOTICE: Getting a group might fail due to removing group.
            mgr <- getGr
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

    cl <- initClient srv hdl
    loop cl


initClient :: Server -> Handle -> IO Client
initClient srv hdl = do
    cl <- newClient hdl
    tick srv $ Log.ClientNew
    clientPut cl $ "/init " <> (expr $ clientId cl) <> "\n"
--    clientPut cl $ "{\"clientId\":" <> (expr $ clientId cl) <> "}\n"
    return cl


notifyClient :: Server -> Group -> Client -> IO () -> IO ()
notifyClient srv@Server{..} gr@Group{..} cl@Client{..} onJoin = do

    -- Notice group to User
    clientPut cl $ "/event join " <> expr groupId <> "\n"
--    clientPut cl $ mconcat
--        [ "{\"event\":\"join-room\"}"
--        , "\n"
--        ]

    onJoin

    runClient srv gr cl

runClient :: Server -> Group -> Client -> IO ()
runClient srv@Server{..} gr@Group{..} cl@Client{..} = do
    let
        broadcastReceiver :: TChan Message -> IO ()
        broadcastReceiver broadcastCh = forever $ do
            atomically $ do
                msg :: Message
                    <- readTChan broadcastCh
                sendMessage cl msg
--            logger $ "BroadcastReceiver works"

        receiver :: IO ()
        receiver = forever $ do
            str <- clientGet srv cl

--            logger $ "Client<" <> (expr clientId) <> "> entered raw strings: " <> expr str
            atomically $ sendMessage cl (Command str)

        server :: IO ()
        server = do

            msg :: Message
                <- atomically $ readTChan clientChan
            continue <- handleMessage srv gr cl msg
            when continue server
            -- Left the room if continue == False.

    broadcastCh <- atomically $ dupTChan groupBroadcastChan

    -- Spawn 3 linked threads.
    race_ (broadcastReceiver broadcastCh) (race_ receiver server)


-- | Add client to group. Returned action is to show latest history.
addClient :: Server -> Client -> Group -> STM (Maybe (IO ()))
addClient Server{..} cl@Client{..} gr@Group{..} = do
    clientMap <- readTVar groupMembers
    cnt <- readTVar groupMemberCount
    gameSt <- getGameStatus gr

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
                when (cnt + 1 == groupCapacity) $ changeGameStatus gr BeforePlay

                -- TODO: Use it later
                hist :: [Message]
                    <- getHistory gr

                sendBroadcast gr (Notice $ "Client<" <> expr clientId <> "> is joined.")

                return $ Just $ do -- IO
                    -- Show history
                    forM_ (reverse hist) $ \msg -> output cl msg
                    tick Log.GroupJoin
                    logger $ mconcat
                        [ "Client<" <> expr clientId <> "> is added to Group<" <> expr groupId <> ">."
                        , " Room members are <" <> expr (cnt + 1) <> ">."
                        ]


removeClient :: Server -> Client -> Group -> STM (IO ())
removeClient srv@Server{..} cl@Client{..} gr@Group{..} = do
    cnt <- readTVar groupMemberCount
    mcl :: Maybe Client
        <- getClient clientId gr
    case mcl of
        Just _ -> do
            modifyTVar' groupMembers (IM.delete clientId)
            modifyTVar' groupMemberCount pred

            sendBroadcast gr (Notice $ "Client<" <> expr clientId <> "> is left.")

            when (cnt == 1) $ deleteGroup srv gr

            return $ do
                clientPut cl $ mconcat
                    [ "/event leave"
                    , "\n"
                    ]
--                    [ "{\"event\":\"leave-room\"}"
--                    , "\n"
--                    ]
                tick Log.GroupLeft
                logger $ mconcat
                    [ "Client<" <> expr clientId <> "> is removed from Group<" <> expr groupId <> ">."
                    , " Room members are <" <> (expr $ cnt - 1) <> ">."
                    ]
        Nothing -> do
            return $ do
                logger $ mconcat
                    [ "Client<" <> expr clientId <> "> doesn't exist in Group<" <> expr groupId <> ">."
                    , " Room members are <" <> expr cnt <> ">."
                    ]

handleMessage :: Server -> Group -> Client -> Message -> IO Bool
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
                    throwIO QuitGame

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
