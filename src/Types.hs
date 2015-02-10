module Types where

import Prelude hiding (log, lookup)

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Map as Map
import qualified Data.Unique as Uniq
import           Data.Monoid

import           System.IO as IO

import qualified Log as Log

------------------------------------------------------------------------------------------
-- | Types

type ClientId = Int
type ClientName = String
type GroupId = Int
type GroupName = String

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
    , logger :: String -> IO ()
    }
data Message
    = Notice String
    | Tell ClientId String
    | Broadcast ClientId String
    | Command String
    deriving Show
--data Game = Game
--    { startTime :: UTCTime
--    , endTime :: UTCTime
--    }


------------------------------------------------------------------------------------------
-- | Server

newServer :: Log.LogChan -> IO Server
newServer logCh = do
    let
        logger str = writeChan logCh str
    gs <- newTVarIO Map.empty

    return $ Server gs logger


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

clientGet :: Client -> IO String
clientGet Client{..} = do
    str <- hGetLine clientHandle
    hFlush clientHandle
    return str

clientPut :: Client -> String -> IO ()
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
        out' (Command str) = return ()
        out' (Broadcast cid str) = hPutStrLn clientHandle $ "Client<" <> show cid <> "> : " <> str
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
                logger $ concat
                    [ "Client<" <> (show $ clientId) <> "> is already joined to Group<" <> (show $ groupId) <> ">."
                    , " Room members are <" <> show cnt <> ">."
                    ]
        else do
            writeTVar groupClients $ Map.insert clientId cl clientMap
            modifyTVar' groupClientCount succ
            cnt :: Int
                <- readTVar groupClientCount
            hist :: [Message]
                <- getHistory gr

            sendBroadcast gr (Notice $ "Client<" <> show clientId <> "> is joined.")

            return $ do
                -- Show history
                forM_ (reverse hist) $ \msg -> output cl msg
                logger $ concat
                    [ "Client<" <> (show $ clientId) <> "> is added to Group<" <> (show $ groupId) <> ">."
                    , " Room members are <" <> show cnt <> ">."
                    ]


removeClient :: Server -> Client -> Group -> STM (IO ())
removeClient Server{..} cl@Client{..} gr@Group{..} = do
    mcl :: Maybe Client
        <- getClient clientId gr
    case mcl of
        Just cl -> do
            modifyTVar' groupClients (Map.delete clientId)
            modifyTVar' groupClientCount pred
            cnt <- readTVar groupClientCount

            sendBroadcast gr (Notice $ "Client<" <> show clientId <> "> is left.")

            return $ do
                logger $ concat
                    [ "Client<" <> (show $ clientId) <> "> is removed from Group<" <> (show $ groupId) <> ">."
                    , " Room members are <" <> show cnt <> ">."
                    ]
        Nothing -> do
            cnt <- readTVar groupClientCount
            return $ do
                logger $ concat
                    [ "Client<" <> (show $ clientId) <> "> doesn't exist in Group<" <> (show $ groupId) <> ">."
                    , " Room members are <" <> show cnt <> ">."
                    ]

