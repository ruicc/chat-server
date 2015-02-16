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
type Timestamp = Int
type GroupCapacity = Int
type PlayTime = Int
type Timeout = Int

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
    , groupPlayTime :: Int -- ^ Seconds
    -- Mutable values
    , groupMembers :: TVar (IM.IntMap Client)
    , groupMemberCount :: TVar Int
    , groupBroadcastChan :: TChan Message -- ^ Write Only channel for group broadcast
    , groupHistory :: TVar [Message]
    , groupGameState :: TVar Game
    , groupCanceler :: TMVar ThreadId
    , groupGameController :: TMVar ThreadId
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

newGroup :: GroupId -> GroupName -> GroupCapacity -> PlayTime -> Timestamp -> Timeout -> STM Group -- STM??? -> Yes. Group(Server) is shared value.
newGroup gid name capacity playTime ts timeout = do
    clientMap <- newTVar IM.empty
    cnt <- newTVar 0
    bch <- newBroadcastTChan
    history <- newTVar []
    gameSt <- newTVar $ Game Waiting []
    mTid <- newEmptyTMVar
    mTid2 <- newEmptyTMVar
    return $ Group
       { groupId             = gid
       , groupName           = name
       , groupCapacity       = capacity
       , groupCreatedAt      = ts
       , groupTimeout        = timeout
       , groupPlayTime       = playTime
       , groupMembers        = clientMap
       , groupMemberCount    = cnt
       , groupBroadcastChan  = bch
       , groupHistory        = history
       , groupGameState      = gameSt
       , groupCanceler       = mTid
       , groupGameController = mTid2
       }

getGroup :: Server -> GroupId -> STM (Maybe Group)
getGroup Server{..} gid = do
    groupMap <- readTVar serverGroups
    return $ IM.lookup gid groupMap

getAllGroups :: Server -> STM [(GroupId, Group)]
getAllGroups Server{..} = do
    groupMap <- readTVar serverGroups
    return $ IM.toList groupMap

createGroup :: Server -> GroupId -> ShortByteString -> GroupCapacity -> PlayTime -> Timestamp -> Timeout -> STM Group
createGroup Server{..} gid name capacity playTime ts timeout = do
    gr <- newGroup gid name capacity playTime ts timeout
    modifyTVar' serverGroups $ IM.insert (groupId gr) gr
    return gr

-- FIXME: basic operation and high-level operation should be separated..
deleteGroup :: Server -> Group -> STM (IO ())
deleteGroup srv@Server{..} gr@Group{..} = do
    members :: [(ClientId, Client)]
        <- IM.toList <$> readTVar groupMembers
    changeGameStatus gr GroupDeleted
    modifyTVar' serverGroups $ IM.delete groupId

    tid <- readTMVar groupCanceler

    return $ do
        forM_ members $ \ (cid, Client{..}) -> do
            throwTo clientThreadId KickedFromRoom -- TODO: Kick理由

        -- Killing the canceler thread must be done at last
        -- so that the canceler thread can call deleteGroup.
        killThread tid

------------------------------------------------------------------------------------------
-- | Client

newClient :: Handle -> IO Client -- STM???
                                 --    -> No. Client is not shared value.
                                 --    -> Client is shared within Server and Group, but STM isn't required.
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
            onRemove <- deleteGroup srv gr
            return $ do
                onRemove
                logger $ "Canceler removed Group<" <> expr groupId <> ">."

        _       -> return $ do
            logger "Canceler fired, but do nothing"
            return ()

changeGameStatus :: Group -> GameStatus -> STM ()
changeGameStatus Group{..} gst = do
    game <- readTVar groupGameState
    writeTVar groupGameState game { gameStatus = gst }

getGameStatus :: Group -> STM GameStatus
getGameStatus Group{..} = do
    game <- readTVar groupGameState
    return $ gameStatus game
