module Types where

import App.Prelude as P

import qualified Data.IntMap as IM
import qualified Data.Unique as Uniq
import qualified Control.Concurrent as Conc

import qualified Log as Log
import           Exception
import           Concurrent

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
    , logger :: ShortByteString -> Concurrent ()
    , tick :: Log.AppEvent -> Concurrent ()
    , errorCollector :: SomeException -> Concurrent ()
    }
data Message
    = Notice ShortByteString
    | Tell ClientId ShortByteString
    | Broadcast ClientId ShortByteString
    | Command ShortByteString
    deriving Show
data Game = Game
    { gameState :: GameState
    , deadClients :: [Client]
    }
data GameState = Waiting | BeforePlay | Playing | Result | GroupDeleted
    deriving (Eq)


------------------------------------------------------------------------------------------
-- | Server

newServer :: Log.LogChan -> Log.StatChan -> Log.ErrorChan -> IO Server
newServer logCh statCh erCh = runCIO return $ do
    let
--        logger str = return ()
        logger str = atomically_ $ writeTChan logCh str
        tick ev = atomically_ $ writeTChan statCh ev
        errorCollector e = atomically_ $ writeTChan erCh e
    gs <- newTVarCIO IM.empty

    return $ Server gs logger tick errorCollector


------------------------------------------------------------------------------------------
-- | Group

newGroup :: GroupId -> GroupName -> GroupCapacity -> PlayTime -> Timestamp -> Timeout -> CSTM r Group -- CSTM r??? -> Yes. Group(Server) is shared value.
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

getGroup :: Server -> GroupId -> CSTM r (Maybe Group)
getGroup Server{..} gid = do
    groupMap <- readTVar serverGroups
    return $ IM.lookup gid groupMap

getAllGroups :: Server -> CSTM r [(GroupId, Group)]
getAllGroups Server{..} = do
    groupMap <- readTVar serverGroups
    return $ IM.toList groupMap

createGroup :: Server -> GroupId -> ShortByteString -> GroupCapacity -> PlayTime -> Timestamp -> Timeout -> CSTM r Group
createGroup Server{..} gid name capacity playTime ts timeout = do
    gr <- newGroup gid name capacity playTime ts timeout
    modifyTVar' serverGroups $ IM.insert (groupId gr) gr
    return gr

-- FIXME: basic operation and high-level operation should be separated..
-- TODO: CSTM r (Concurrent ())
deleteGroup :: Server -> Group -> CSTM r (Concurrent ())
deleteGroup srv@Server{..} gr@Group{..} = do
    members :: [(ClientId, Client)]
        <- IM.toList <$> readTVar groupMembers
    changeGameState gr GroupDeleted
    modifyTVar' serverGroups $ IM.delete groupId

    tid <- readTMVar groupCanceler

    return $ do -- CIO
        forM_ members $ \ (cid, Client{..}) -> do
            throwTo clientThreadId KickedFromRoom -- TODO: Kick理由

        -- Killing the canceler thread must be done at last
        -- so that the canceler thread can call deleteGroup.
        killThread tid

------------------------------------------------------------------------------------------
-- | Client

newClient :: Handle -> Concurrent Client -- CSTM r???
                                 --    -> No. Client is not shared value.
                                 --    -> Client is shared within Server and Group, but CSTM r isn't required.
newClient hdl = do
    cid :: Int
        <- Uniq.hashUnique <$> liftIO Uniq.newUnique
    ch :: TChan Message
        <- newTChanCIO
    tid <- myThreadId
    return $ Client
        { clientId = cid
        , clientHandle = hdl
        , clientChan = ch
        , clientThreadId = tid
        }

clientGet :: Server -> Client -> Concurrent ShortByteString
clientGet Server{..} Client{..} = do
    str <- rstrip <$> (liftIO $ hGetLine clientHandle)
    liftIO $ hFlush clientHandle
--    logger $ "(raw) " <> str
    return str

clientPut :: Client -> ShortByteString -> Concurrent ()
clientPut Client{..} str = do
    liftIO $ hPutStr clientHandle str
    liftIO $ hFlush clientHandle


------------------------------------------------------------------------------------------
-- | Message

sendMessage :: Client -> Message -> CSTM r ()
sendMessage Client{..} msg = do
    writeTChan clientChan msg


sendBroadcast :: Group -> Message -> CSTM r ()
sendBroadcast gr@Group{..} msg = do
    addHistory gr msg
    writeTChan groupBroadcastChan msg


addHistory :: Group -> Message -> CSTM r ()
addHistory Group{..} msg = do
    hist <- readTVar groupHistory
    if length hist >= 20
        -- Keep only latest 20 messages
        then writeTVar groupHistory $ msg : take 19 hist
        else writeTVar groupHistory $ msg : hist

getHistory :: Group -> CSTM r [Message]
getHistory Group{..} = readTVar groupHistory


output :: Client -> Message -> Concurrent ()
output Client{..} msg = do
    let
        out' (Command _) = return ()
        out' (Broadcast cid str) = hPutStrLn clientHandle $ "Client<" <> expr cid <> "> : " <> str
        out' (Notice str) = hPutStrLn clientHandle $ str
        out' _ = error "Not impl yet"
    liftIO $ out' msg


------------------------------------------------------------------------------------------
-- | Group and Client

getClient :: ClientId -> Group -> CSTM r (Maybe Client)
getClient cid Group{..} = do
    clientMap <- readTVar groupMembers
    return $ IM.lookup cid clientMap


cancelWaiting :: Server -> Group -> Concurrent ()
cancelWaiting srv@Server{..} gr@Group{..} = join $ atomically_ $ do -- CSTM
    -- Check GameState
    gameSt <- gameState <$> readTVar groupGameState
    case gameSt of
        Waiting -> do
            onRemove <- deleteGroup srv gr
            return $ do -- CIO
                onRemove
                logger $ "Canceler removed Group<" <> expr groupId <> ">."

        _       -> return $ do
            logger "Canceler fired, but do nothing"
            return ()

changeGameState :: Group -> GameState -> CSTM r ()
changeGameState Group{..} gst = do
    game <- readTVar groupGameState
    writeTVar groupGameState game { gameState = gst }

getGameState :: Group -> CSTM r GameState
getGameState Group{..} = do
    game <- readTVar groupGameState
    return $ gameState game
