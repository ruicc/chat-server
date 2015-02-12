module Server (runClientThread) where

import           App.Prelude

import           Data.List (intersperse)

import qualified Log as Log
import           Types



runClientThread :: Server -> Handle -> IO ()
runClientThread srv@Server{..} hdl = do
    let
        loop cl = do

            -- Group manupilations
            mgr :: Maybe Group
                <- groupOperations srv cl

            case mgr of
                Just gr -> do
                    -- NOTICE: Modify a shared value(gr/srv), need to notify it to Client.
                    -- "mask" prevents to catch async-exceptions between "addClient" to "notifyClient".
                    _e :: Either SomeException ()
                        <- try $ mask $ \restore -> do
                            exprHistory <- atomically $ addClient srv cl gr
                            restore (notifyClient srv gr cl exprHistory) `finally` (join $ atomically $ removeClient srv cl gr)
--                    logger "Hey, select a room again"
                    loop cl

                Nothing -> do
                    tick Log.ClientLeft
--                    clientPut cl $ "Good bye!\n"
    cl <- initClient srv hdl
    loop cl

initClient :: Server -> Handle -> IO Client
initClient srv hdl = do
    cl <- newClient hdl
    tick srv $ Log.ClientNew
    clientPut cl $ "{\"clientId\":" <> (expr $ clientId cl) <> "}\n"
    return cl


groupOperations :: Server -> Client -> IO (Maybe Group)
groupOperations srv@Server{..} cl@Client{..} = do
    grs :: [(GroupId, Group)]
        <- atomically $ getAllGroups srv

    clientPut cl $ mconcat
        [ "{\"rooms\":["
        , mconcat $ intersperse "," $ map (expr . fst) grs
        , "],"
        , "\"status\":\"groupSelect\""
        , "}"
        , "\n"
        ]

    input :: ShortByteString
        <- rstrip <$> clientGet cl

--    logger $ "Group select: " <> expr input

    case words input of
        ["/quit"] -> return Nothing

        ["/new", gid'] -> do
            case readInt gid' of
                Nothing -> do
                    groupOperations srv cl
                Just (gid, _) -> do
                    mgr <- atomically $ getGroup srv gid
                    case mgr of
                        Nothing -> do
                            _gr <- atomically $ createGroup srv gid
                            tick Log.GroupNew
                            groupOperations srv cl
                        Just _ -> groupOperations srv cl

        ["/join", gid'] -> do

            case readInt gid' of
                Nothing -> do
                    groupOperations srv cl
                Just (gid, _) -> do
                    mgr <- atomically $ getGroup srv gid
                    case mgr of
                        Nothing -> do
                            gr <- atomically $ createGroup srv gid
                            tick Log.GroupNew
                            return $ Just gr
                        Just gr -> return $ Just gr
        _ -> groupOperations srv cl


notifyClient :: Server -> Group -> Client -> IO () -> IO ()
notifyClient srv@Server{..} gr@Group{..} cl@Client{..} onJoin = do

    -- Notice group to User
    clientPut cl $ mconcat
        [ "{\"status\":\"joined\"}"
        , "\n"
        ]
--        [ "\n"
--        , "Hello, this is easy-chat.\n"
--        , "Your ClientId is <" <> expr clientId <> ">,\n"
--        , "Your GroupId is <" <> expr groupId <> ">.\n"
--        , "Type \"/quit\" when you quit.\n\n"
--        ]

--    onJoin

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
            str' <- clientGet cl

            let str = rstrip str' -- Chop newline
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


handleMessage :: Server -> Group -> Client -> Message -> IO Bool
handleMessage Server{..} gr@Group{..} cl@Client{..} msg = do
    -- Send message to client
    output cl msg

    case msg of
        Command str -> do
            case words str of
                ["/quit"] -> do
--                    clientPut cl $ "You left room.\n"
                    return False
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
