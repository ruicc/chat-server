module Types where

import App.Prelude

import qualified Data.IntMap as IM
import qualified Data.Unique as Uniq

import qualified Log as Log
import           Exception

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
    , clientThreadId :: ThreadId
    }
data Group = Group
    { groupId :: GroupId
    , groupName :: GroupName
    , groupCapacity :: Int
    , groupCreatedAt :: Int -- ^ UnixTime
    , groupTimeout :: Int -- ^ Seconds

    , groupMembers :: TVar (IM.IntMap Client)
    , groupMemberCount :: TVar Int
    , groupBroadcastChan :: TChan Message -- ^ Write Only channel for group broadcast
    , groupHistory :: TVar [Message]
    , groupGameState :: TVar Game
    , groupCanceler :: TMVar ThreadId
    }
data Server = Server
    { serverGroups :: TVar (IM.IntMap Group)
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
data Game = Game
    { gameStatus :: GameStatus
    , deadClients :: [Client]
    }
data GameStatus = Waiting | BeforePlay | Playing | Result | GroupDeleted
    deriving (Eq)

newtype GroupCapacity = GroupCapacity Int
    deriving (Show, Read, Eq, Ord, Num)

type Timestamp = Int

------------------------------------------------------------------------------------------
-- | Server

newServer :: Log.LogChan -> Log.StatChan -> Log.ErrorChan -> IO Server
newServer logCh statCh erCh = do
    let
--        logger str = return ()
        logger str = atomically $ writeTChan logCh str
        tick ev = atomically $ writeTChan statCh ev
        errorCollector e = atomically $ writeTChan erCh e
    gs <- newTVarIO IM.empty

    return $ Server gs logger tick errorCollector


------------------------------------------------------------------------------------------
-- | Group

newGroup :: GroupId -> ShortByteString -> Int -> Timestamp -> Int -> STM Group -- STM??? -> Yes. Group(Server) is shared value.
newGroup gid name capacity ts timeout = do
    clientMap <- newTVar IM.empty
    cnt <- newTVar 0
    bch <- newBroadcastTChan
    history <- newTVar []
    gameSt <- newTVar $ Game Waiting []
    mTid <- newEmptyTMVar
    return $ Group
            gid
            name
            capacity
            ts
            timeout
            clientMap
            cnt
            bch
            history
            gameSt
            mTid

getGroup :: Server -> GroupId -> STM (Maybe Group)
getGroup Server{..} gid = do
    groupMap <- readTVar serverGroups
    return $ IM.lookup gid groupMap

getAllGroups :: Server -> STM [(GroupId, Group)]
getAllGroups Server{..} = do
    groupMap <- readTVar serverGroups
    return $ IM.toList groupMap

createGroup :: Server -> GroupId -> ShortByteString -> GroupCapacity -> Int -> Int -> STM Group
createGroup Server{..} gid name (GroupCapacity capacity) ts timeout = do
    gr <- newGroup gid name capacity ts timeout
    modifyTVar' serverGroups $ IM.insert (groupId gr) gr
    return gr

deleteGroup :: Server -> Group -> STM ()
deleteGroup Server{..} Group{..} = do
    modifyTVar' serverGroups $ IM.delete groupId


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
    tid <- myThreadId
    return $ Client
        { clientId = cid
        , clientHandle = hdl
        , clientChan = ch
        , clientThreadId = tid
        }

clientGet :: Server -> Client -> IO ShortByteString
clientGet Server{..} Client{..} = do
    str <- rstrip <$> hGetLine clientHandle
    hFlush clientHandle
--    logger $ "(raw) " <> str
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
    clientMap <- readTVar groupMembers
    return $ IM.lookup cid clientMap


cancelWaiting :: Server -> Group -> IO ()
cancelWaiting srv@Server{..} gr@Group{..} = join $ atomically $ do
    -- Check GameStatus
    gameSt <- gameStatus <$> readTVar groupGameState
    case gameSt of
        Waiting -> do
            members :: [(ClientId, Client)]
                <- IM.toList <$> readTVar groupMembers
            changeGameStatus gr GroupDeleted
            deleteGroup srv gr

            return $ do
                logger $ "CancelWaiting fired. Group<" <> expr groupId <> "> is removed"
                forM_ members $ \ (cid, Client{..}) -> do
                    logger $ "CancelWaiting: " <> expr cid
                    throwTo clientThreadId KickedFromRoom -- TODO: Kick理由
        _ -> return $ do
            logger "CancelWaiting fired, but do nothing"
            return ()

changeGameStatus :: Group -> GameStatus -> STM ()
changeGameStatus Group{..} gst = do
    game <- readTVar groupGameState
    writeTVar groupGameState game { gameStatus = gst }

getGameStatus :: Group -> STM GameStatus
getGameStatus Group{..} = do
    game <- readTVar groupGameState
    return $ gameStatus game
