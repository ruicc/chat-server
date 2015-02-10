module Client (clientProcess) where

import Prelude hiding (log, lookup)

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception

import qualified Data.Unique as Uniq
import           Data.List (intersperse)
import           Data.Monoid

import           System.IO as IO

import           Types
import           Utils (rstrip)

clientProcess :: Server -> Client -> IO ()
clientProcess srv@Server{..} cl@Client{..} = do
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
                            showHistory <- atomically $ addClient srv cl gr
                            restore (notifyClient srv cl gr showHistory) `finally`
                                    (atomically $ removeClient srv cl gr)

                    loop

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


notifyClient :: Server -> Client -> Group -> IO () -> IO ()
notifyClient srv@Server{..} cl@Client{..} gr@Group{..} onJoin = do

    -- Notice group to User
    hPutStrLn clientHandle $ concat
        [ "\n"
        , "Hello, this is easy-chat.\n"
        , "Your ClientId is <" <> show clientId <> ">,\n"
        , "Your GroupId is <" <> show groupId <> ">.\n"
        , "Type \"/quit\" when you quit.\n"
        ]

    onJoin

    runClient srv cl gr

runClient :: Server -> Client -> Group -> IO ()
runClient srv@Server{..} cl@Client{..} gr@Group{..} = do
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

    broadcastCh <- atomically $ dupTChan groupBroadcastChan

    -- Spawn 3 linked threads.
    race_ (broadcastReceiver broadcastCh) (race_ receiver server)

    -- Thread which accepts a request terminated here.
    return ()


handleMessage :: Server -> Group -> Client -> Message -> IO Bool
handleMessage srv  gr@Group{..} cl@Client{..} msg = do
    output cl msg

    case msg of
        Command str -> do
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
            return True

        Notice str -> do
            return True

        _ -> error "Not impl yet"
