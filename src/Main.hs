module Main where


import Prelude hiding (log, lookup)
import Network

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception

import qualified Data.Map as Map
import qualified Data.Unique as Uniq
import           Data.Monoid
import           Data.List (intersperse)
import           Data.Char (isSpace)
import           Text.Printf (printf)

import           System.IO as IO

main :: IO ()
main = withSocketsDo $ do
    let
        port = 3000 :: Int

    logCh <- spawnLogger
    server <- newServer logCh

    socket <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port

    forever $ do
        (hdl, hostname, portnumber) <- accept socket
        printf "Accepted from %s\n" hostname

        cl <- newClient hdl

        forkFinally (clientProcess server cl) (\ _ -> hClose hdl)


clientProcess :: Server -> Client -> IO ()
clientProcess srv cl@Client{..} = do

    hSetBuffering clientHandle LineBuffering
--    hSetBuffering clientHandle NoBuffering

    let
        loop = do

            -- Group manupilations
            mgr <- groupOperations srv cl

            case mgr of
                Just gr -> do
                    -- NOTICE: Modify a shared value(gr/srv), need to notify it to Client.
                    -- "mask" prevents to catch async-exceptions between "addClient" to "notifyClient".
                    e :: Either SomeException ()
                        <- try $ mask $ \restore -> do
                            addClient srv cl gr
                            restore (notifyClient srv cl gr) --`finally` removeClient srv cl gr

                    case e of
                        Left e -> do
                            removeClient srv cl gr
                            loop
                        Right _ -> do
                            error "ここにもこない"
                Nothing -> do
                    hPutStrLn clientHandle $ "Good bye!"
    loop



groupOperations :: Server -> Client -> IO (Maybe Group)
groupOperations srv@Server{..} cl@Client{..} = do
    let
        getLine = do
            str <- hGetLine clientHandle
            hFlush clientHandle
            return str
        putStr str = do
            hPutStr clientHandle str
            hFlush clientHandle

    grs :: [(GroupId, Group)]
        <- atomically $ getAllGroups srv

    if null grs
        then do
            putStr $ "There are no chat rooms now.\nCreating new one ...\n"
            gid :: GroupId
                <- Uniq.hashUnique <$> Uniq.newUnique
            _gr <- atomically $ createGroup srv gid
            groupOperations srv cl
            
        else do
            putStr $ concat
                [ "Current rooms: " <> (concat $ intersperse "," $ map (show . fst) grs) <> "\n"
                ,"Select one, \"/new\" or \"/quit\"> "
                ]

            input :: String
                <- rstrip <$> getLine

            logger $ "Group ops: " <> show input

            case input of
                "/new" -> do
                    gid :: GroupId
                        <- Uniq.hashUnique <$> Uniq.newUnique
                    _gr <- atomically $ createGroup srv gid
                    groupOperations srv cl

                "/quit" -> do
                    return Nothing

                _ -> do
                
                    eInt :: Either SomeException Int
                        <- try $ readIO input
                    case eInt of
                        Left e -> do
                            logger $ show e
                            groupOperations srv cl
                        Right gid -> do
                            mgr <- atomically $ getGroup srv gid
                            case mgr of
                                Nothing -> groupOperations srv cl
                                Just gr -> return $ Just gr


notifyClient :: Server -> Client -> Group -> IO ()
notifyClient srv@Server{..} cl@Client{..} gr@Group{..} = do

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
            str' <- hGetLine clientHandle
            hFlush clientHandle

            let str = rstrip str' -- Chop newline
            logger $ "Client<" <> (show clientId) <> "> entered raw strings: " <> show str
            atomically $ sendMessage cl (Command str)

        server :: IO ()
        server = do

            msg :: Message
                <- atomically $ readTChan clientChan
            continue <- handleMessage srv gr cl msg
            when continue server
            -- Return..
            throwIO $ ErrorCall "Left room."

    -- Notice group to User
    hPutStrLn clientHandle $ concat
        [ "\n"
        , "Hello, this is easy-chat.\n"
        , "Your ClientId is <" <> show clientId <> ">,\n"
        , "Your GroupId is <" <> show groupId <> ">.\n"
        , "Type \"/quit\" when you quit.\n"
        ]

    broadcastCh <- atomically $ dupTChan groupBroadcastChan

    -- Spawn 3 linked threads.
    race_ (broadcastReceiver broadcastCh) (race_ receiver server)

    -- Thread which accepts a request terminated here.
    return ()

-- Interpret messages
handleMessage :: Server -> Group -> Client -> Message -> IO Bool
handleMessage srv  gr@Group{..} Client{..} msg = do
    case msg of
        Command str -> do
--            hPutStrLn clientHandle $ "Command: " <> str
            case words str of
                ["/quit"] -> do
                    hPutStrLn clientHandle $ "You left room."
                    return False
                [] -> do
                    -- Ignore empty messages.
                    return True
                _ -> do
                    atomically $ sendBroadcast gr (Broadcast clientId str)
                    return True
        Broadcast cid str -> do
            hPutStrLn clientHandle $ "Client<" <> show cid <> "> : " <> str
            return True

        Notice str -> do
            hPutStrLn clientHandle $ str
            return True

        _ -> throwIO $ ErrorCall "Not impl yet"

------------------------------------------------------------------------------------------

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
--    , groupHistory :: TVar [Message]
    , groupBroadcastChan :: TChan Message -- ^ Write Only channel for group broadcast
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

newClient :: Handle -> IO Client -- TODO: STM??? -> No. Client is not shared value.
newClient hdl = do
    cid :: Int
        <- Uniq.hashUnique <$> Uniq.newUnique
    ch :: TChan Message
        <- newTChanIO
    return $ Client cid hdl ch

newGroup :: GroupId -> STM Group -- STM??? -> Yes. Group(Server) is shared value.
newGroup gid = do
    tv <- newTVar Map.empty
    cnt <- newTVar 0
    bch <- newBroadcastTChan
    return $ Group gid tv cnt bch

newServer :: LogChan -> IO Server
newServer logCh = do
    let
        logger str = writeChan logCh str
    gs <- newTVarIO Map.empty

    return $ Server gs logger

------------------------------------------------------------------------------------------

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = do
    writeTChan clientChan msg

sendBroadcast :: Group -> Message -> STM ()
sendBroadcast gr@Group{..} msg = do
    writeTChan groupBroadcastChan msg


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

------------------------------------------------------------------------------------------

getClient :: ClientId -> Group -> STM (Maybe Client)
getClient cid Group{..} = do
    clientMap <- readTVar groupClients
    return $ Map.lookup cid clientMap


addClient :: Server -> Client -> Group -> IO ()
addClient Server{..} cl@Client{..} gr@Group{..} = do
    join $ atomically $ do
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
                cnt <- readTVar groupClientCount

                sendBroadcast gr (Notice $ "Client<" <> show clientId <> "> is joined.")

                return $ do
                    logger $ concat
                        [ "Client<" <> (show $ clientId) <> "> is added to Group<" <> (show $ groupId) <> ">."
                        , " Room members are <" <> show cnt <> ">."
                        ]


removeClient :: Server -> Client -> Group -> IO ()
removeClient Server{..} cl@Client{..} gr@Group{..} = do
    join $ atomically $ do
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

deleteGroup :: Server -> Group -> IO ()
deleteGroup Server{..} Group{..} = atomically $ do
    modifyTVar' serverGroups $ Map.delete groupId



------------------------------------------------------------------------------------------

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse
