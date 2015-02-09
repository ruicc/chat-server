module Main where


import Prelude hiding (log, lookup)
import Network
import qualified Network.Socket as Socket

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception

import qualified Data.Map as Map
import qualified Data.Unique as Uniq
import           Data.Monoid ((<>))
import           System.IO as IO
--import Data.ByteString.Char8 (putStrLn)
import Data.ByteString.UTF8 (fromString)
import Text.Printf (printf)

main :: IO ()
main = withSocketsDo $ do
    let
        port = 3000 :: Int

    logCh <- spawnLogger
    server <- newServer

    socket <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port

    forever $ do
        (handle, hostname, portnumber) <- accept socket
        printf "Accepted from %s\n" hostname
        forkFinally (clientProcess logCh handle server) (\ _ -> hClose handle)

echo :: LogChan -> Handle -> Server -> IO ()
echo logCh h server = forever $ do
    hSetBuffering h LineBuffering
    str <- hGetLine h
    log logCh str
    hPutStrLn h str

clientProcess :: LogChan -> Handle -> Server -> IO ()
clientProcess logCh hdl srv = do
    let
        gid :: GroupId
            = 23

    hSetBuffering hdl LineBuffering

    -- Client initialization
    cl <- newClient

    -- Group manupilations
    gr <- atomically $ do
        mg <- getGroup srv gid
        case mg of
            Just gr -> return gr
            Nothing -> createGroup srv gid

    -- NOTICE: Modify a shared value(gr/srv), need to notify it to Client.
    -- "mask" prevents to catch async-exceptions between "addClient" to "notifyClient".
    mask $ \restore -> do
        addClient logCh cl gr
        restore (notifyClient logCh hdl cl gr) `finally` removeClient logCh cl gr


notifyClient :: LogChan -> Handle -> Client -> Group -> IO ()
notifyClient logCh hdl cl@Client{..} gr@Group{..} = do
    -- Notice group to User
    hPutStrLn hdl $ "Your ClientId is <" <> show clientId <> ">,"
    hPutStrLn hdl $ "Your GroupId is <" <> show groupId <> ">."

    let
        receiver :: IO ()
        receiver = forever $ do
            str' <- hGetLine hdl
            let str = init str' -- Chop newline
            atomically $ sendMessage cl (Command str)
            log logCh $ "Client<" <> (show clientId) <> "> sent Message \"" <> str <> "\""

        sender :: IO ()
        sender = forever $ do
            msg :: Message
                <- atomically $ readTChan clientChan
            handleMessage logCh hdl msg

    -- Spawn 2 linked threads.
    race_ receiver sender

    -- Thread which accepts a request terminated here.
    return ()


handleMessage :: LogChan -> Handle -> Message -> IO ()
handleMessage logCh hdl msg = do
    case msg of
        Command str -> do
            hPutStrLn hdl $ "Command: " <> str
        _ -> throwIO $ ErrorCall "Not impl yet"

------------------------------------------------------------------------------------------

type ClientId = Int
type ClientName = String
type GroupId = Int
type GroupName = String

data Client = Client
    { clientId :: ClientId
--    , clientName :: ClientName
    , clientChan :: TChan Message
    }
data Group = Group
    { groupId :: GroupId
--    , groupName :: GroupName
    , groupClients :: TVar (Map.Map ClientId Client)
    , groupClientCount :: TVar Int
    }
data Server = Server
    { serverGroups :: TVar (Map.Map GroupId Group)
    }
data Message
    = Notice String
    | Tell ClientId String
    | Broadcast ClientId String
    | Command String

newClient :: IO Client -- TODO: STM??? -> No. Client is not shared value.
newClient = do
    id <- Uniq.hashUnique <$> Uniq.newUnique
    ch <- newTChanIO
    return $ Client id ch

newGroup :: GroupId -> STM Group -- STM??? -> Yes. Group(Server) is shared value.
newGroup gid = do
    tv <- newTVar Map.empty
    cnt <- newTVar 0
    return $ Group gid tv cnt

newServer :: IO Server
newServer = do
    gs <- newTVarIO Map.empty
    return $ Server gs

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = do
    writeTChan clientChan msg

------------------------------------------------------------------------------------------

type LogChan = Chan String

spawnLogger :: IO LogChan
spawnLogger = do
    let
        supervisor ch = do
            as :: Async ()
                <- async (logger ch)
            res :: Either SomeException ()
                <- try $ wait as
            case res of
                Left e -> do
                    -- Error reporting..
                    supervisor ch
                Right _ -> error "ここには来ない"

        logger :: LogChan -> IO ()
        logger ch = forever $ do
            str <- readChan ch
            putStrLn $ "Log: " <> str

    ch :: LogChan
        <- newChan

    _tid <- forkIO $ supervisor ch
    return ch

log :: LogChan -> String -> IO ()
log logCh str = writeChan logCh str

------------------------------------------------------------------------------------------

getClient :: ClientId -> Group -> STM (Maybe Client)
getClient cid Group{..} = do
    clientMap <- readTVar groupClients
    return $ Map.lookup cid clientMap


addClient :: LogChan -> Client -> Group -> IO ()
addClient logCh cl@Client{..} gr@Group{..} = do
    join $ atomically $ do
        clientMap <- readTVar groupClients
        if Map.member clientId clientMap
            then do
                cnt <- readTVar groupClientCount
                return $ do
                    log logCh $ concat
                        [ "Client<" <> (show $ clientId) <> "> is already joined to Group<" <> (show $ groupId) <> ">."
                        , " Room members are <" <> show cnt <> ">."
                        ]
            else do
                writeTVar groupClients $ Map.insert clientId cl clientMap
                modifyTVar' groupClientCount succ
                cnt <- readTVar groupClientCount

                return $ do
                    log logCh $ concat
                        [ "Client<" <> (show $ clientId) <> "> is added to Group<" <> (show $ groupId) <> ">."
                        , " Room members are <" <> show cnt <> ">."
                        ]


removeClient :: LogChan -> Client -> Group -> IO ()
removeClient logCh cl@Client{..} gr@Group{..} = do
    join $ atomically $ do
        mcl :: Maybe Client
            <- getClient clientId gr
        case mcl of
            Just cl -> do
                modifyTVar' groupClients (Map.delete clientId)
                modifyTVar' groupClientCount pred
                cnt <- readTVar groupClientCount
                return $ do
                    log logCh $ concat
                        [ "Client<" <> (show $ clientId) <> "> is removed from Group<" <> (show $ groupId) <> ">."
                        , " Room members are <" <> show cnt <> ">."
                        ]
            Nothing -> do
                cnt <- readTVar groupClientCount
                return $ do
                    log logCh $ concat
                        [ "Client<" <> (show $ clientId) <> "> doesn't exist in Group<" <> (show $ groupId) <> ">."
                        , " Room members are <" <> show cnt <> ">."
                        ]


getGroup :: Server -> GroupId -> STM (Maybe Group)
getGroup Server{..} gid =do
    groupMap <- readTVar serverGroups
    return $ Map.lookup gid groupMap


createGroup :: Server -> GroupId -> STM Group
createGroup Server{..} gid = do
    gr <- newGroup gid
    modifyTVar' serverGroups $ Map.insert (groupId gr) gr
    return gr

deleteGroup :: Server -> Group -> IO ()
deleteGroup Server{..} Group{..} = atomically $ do
    modifyTVar' serverGroups $ Map.delete groupId


