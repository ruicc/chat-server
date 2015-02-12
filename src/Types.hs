module Types where

import App.Prelude

import qualified Data.Map as Map
import qualified Data.Unique as Uniq

import qualified Log as Log

------------------------------------------------------------------------------------------
-- | Types

type ClientId = Int
type ClientName = ShortByteString
type GroupId = Int
type GroupName = ShortByteString

data Client = Client
    { clientId :: ClientId
--    , clientName :: ClientName
    , clientHandle :: Handle
    , clientChan :: TChan Message -- ^ Mailbox sent to this client
    }
data Group = Group
    { groupId :: GroupId
--    , groupName :: GroupName
    , groupClients :: TVar (Map.Map ClientId Client)
    , groupClientCount :: TVar Int
    , groupBroadcastChan :: TChan Message -- ^ Write Only channel for group broadcast
    , groupHistory :: TVar [Message]
--    , groupGameState :: TVar Game
    }
data Server = Server
    { serverGroups :: TVar (Map.Map GroupId Group)
    , logger :: ShortByteString -> IO ()
    , tick :: Log.AppEvent -> IO ()
    , errorCollector :: SomeException -> IO ()
    }
data Message
    = Notice ShortByteString
    | Tell ClientId ShortByteString
    | Broadcast ClientId ShortByteString
    | Command ShortByteString
    deriving Show
--data Game = Game
--    { startTime :: UTCTime
--    , endTime :: UTCTime
--    }


------------------------------------------------------------------------------------------
-- | Server

newServer :: Log.LogChan -> Log.StatChan -> Log.ErrorChan -> IO Server
newServer logCh statCh erCh = do
    let
--        logger str = return ()
        logger str = atomically $ writeTChan logCh str
        tick ev = atomically $ writeTChan statCh ev
        errorCollector e = atomically $ writeTChan erCh e
    gs <- newTVarIO Map.empty

    return $ Server gs logger tick errorCollector


------------------------------------------------------------------------------------------
-- | Group

newGroup :: GroupId -> STM Group -- STM??? -> Yes. Group(Server) is shared value.
newGroup gid = do
    clientMap <- newTVar Map.empty
    cnt <- newTVar 0
    history <- newTVar []
    bch <- newBroadcastTChan
    return $ Group gid clientMap cnt bch history

getGroup :: Server -> GroupId -> STM (Maybe Group)
getGroup Server{..} gid = do
    groupMap <- readTVar serverGroups
    return $ Map.lookup gid groupMap

getAllGroups :: Server -> STM [(GroupId, Group)]
getAllGroups Server{..} = do
    groupMap <- readTVar serverGroups
    return $ Map.toList groupMap

createGroup :: Server -> GroupId -> STM Group
createGroup Server{..} gid = do
    gr <- newGroup gid
    modifyTVar' serverGroups $ Map.insert (groupId gr) gr
    return gr

deleteGroup :: Server -> Group -> STM ()
deleteGroup Server{..} Group{..} = do
    modifyTVar' serverGroups $ Map.delete groupId


------------------------------------------------------------------------------------------
-- | Client

newClient :: Handle -> IO Client -- STM???
                                 --    -> No. Client is not shared value.
                                 --    -> Client is shared with server and client but STM isn't required.
newClient hdl = do
    cid :: Int
        <- Uniq.hashUnique <$> Uniq.newUnique
    ch :: TChan Message
        <- newTChanIO
    return $ Client
        { clientId = cid
        , clientHandle = hdl
        , clientChan = ch
        }

clientGet :: Client -> IO ShortByteString
clientGet Client{..} = do
    str <- hGetLine clientHandle
    hFlush clientHandle
    return str

clientPut :: Client -> ShortByteString -> IO ()
clientPut Client{..} str = do
    hPutStr clientHandle str
    hFlush clientHandle


------------------------------------------------------------------------------------------
-- | Message

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = do
    writeTChan clientChan msg


sendBroadcast :: Group -> Message -> STM ()
sendBroadcast gr@Group{..} msg = do
    addHistory gr msg
    writeTChan groupBroadcastChan msg


addHistory :: Group -> Message -> STM ()
addHistory Group{..} msg = do
    hist <- readTVar groupHistory
    if length hist >= 20
        -- Keep only latest 20 messages
        then writeTVar groupHistory $ msg : take 19 hist
        else writeTVar groupHistory $ msg : hist

getHistory :: Group -> STM [Message]
getHistory Group{..} = readTVar groupHistory


output :: Client -> Message -> IO ()
output Client{..} msg = do
    let
        out' (Command _) = return ()
        out' (Broadcast cid str) = hPutStrLn clientHandle $ "Client<" <> expr cid <> "> : " <> str
        out' (Notice str) = hPutStrLn clientHandle $ str
        out' _ = error "Not impl yet"
    out' msg


------------------------------------------------------------------------------------------
-- | Group and Client

getClient :: ClientId -> Group -> STM (Maybe Client)
getClient cid Group{..} = do
    clientMap <- readTVar groupClients
    return $ Map.lookup cid clientMap


-- | Add client to group. Returned action is to show latest history.
addClient :: Server -> Client -> Group -> STM (IO ())
addClient Server{..} cl@Client{..} gr@Group{..} = do
    clientMap <- readTVar groupClients
    if Map.member clientId clientMap
        then do
            cnt <- readTVar groupClientCount
            return $ do
                logger $ mconcat
                    [ "Client<" <> expr clientId <> "> is already joined to Group<" <> expr groupId <> ">."
                    , " Room members are <" <> expr cnt <> ">."
                    ]
        else do
            writeTVar groupClients $ Map.insert clientId cl clientMap
            modifyTVar' groupClientCount succ
            cnt :: Int
                <- readTVar groupClientCount
            hist :: [Message]
                <- getHistory gr

            sendBroadcast gr (Notice $ "Client<" <> expr clientId <> "> is joined.")

            return $ do
                -- Show history
--                forM_ (reverse hist) $ \msg -> output cl msg
                tick Log.GroupJoin
                logger $ mconcat
                    [ "Client<" <> expr clientId <> "> is added to Group<" <> expr groupId <> ">."
                    , " Room members are <" <> expr cnt <> ">."
                    ]


removeClient :: Server -> Client -> Group -> STM (IO ())
removeClient Server{..} Client{..} gr@Group{..} = do
    mcl :: Maybe Client
        <- getClient clientId gr
    case mcl of
        Just _ -> do
            modifyTVar' groupClients (Map.delete clientId)
            modifyTVar' groupClientCount pred
            cnt <- readTVar groupClientCount

            sendBroadcast gr (Notice $ "Client<" <> expr clientId <> "> is left.")

            return $ do
                tick Log.GroupLeft
                logger $ mconcat
                    [ "Client<" <> expr clientId <> "> is removed from Group<" <> expr groupId <> ">."
                    , " Room members are <" <> expr cnt <> ">."
                    ]
        Nothing -> do
            cnt <- readTVar groupClientCount
            return $ do
                logger $ mconcat
                    [ "Client<" <> expr clientId <> "> doesn't exist in Group<" <> expr groupId <> ">."
                    , " Room members are <" <> expr cnt <> ">."
                    ]

