module Client (clientProcess) where

import App.Prelude

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception

import qualified Data.Unique as Uniq
import           Data.List (intersperse)
import           Data.Monoid

import qualified Log as Log
import           Types

clientProcess :: Server -> Client -> IO ()
clientProcess srv@Server{..} cl@Client{..} = do
    let
        loop = do

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

                    loop

                Nothing -> do
                    tick Log.ClientLeft
                    clientPut cl $ "Good bye!\n"
    loop



groupOperations :: Server -> Client -> IO (Maybe Group)
groupOperations srv@Server{..} cl@Client{..} = do
    grs :: [(GroupId, Group)]
        <- atomically $ getAllGroups srv

    if null grs
        then do
            clientPut cl $ "There are no chat rooms now.\nCreating new one ...\n"
            gid :: GroupId
                <- Uniq.hashUnique <$> Uniq.newUnique
            _gr <- atomically $ createGroup srv gid
            groupOperations srv cl
            
        else do
            clientPut cl $ mconcat
                [ "Current rooms: " <> (mconcat $ intersperse "," $ map (expr . fst) grs) <> "\n"
                ,"Select one, \"/new\" or \"/quit\"> "
                ]

            input :: ShortByteString
                <- rstrip <$> clientGet cl

            logger $ "Group ops: " <> expr input

            case input of
                "/new" -> do
                    gid :: GroupId
                        <- Uniq.hashUnique <$> Uniq.newUnique
                    _gr <- atomically $ createGroup srv gid
                    tick Log.GroupNew

                    groupOperations srv cl

                "/quit" -> do
                    return Nothing

                _ -> do
                
                    case readInt input of
                        Nothing -> do
                            groupOperations srv cl
                        Just (gid, _) -> do
                            mgr <- atomically $ getGroup srv gid
                            case mgr of
                                Nothing -> groupOperations srv cl
                                Just gr -> return $ Just gr


notifyClient :: Server -> Group -> Client -> IO () -> IO ()
notifyClient srv@Server{..} gr@Group{..} cl@Client{..} onJoin = do

    -- Notice group to User
    clientPut cl $ mconcat
        [ "\n"
        , "Hello, this is easy-chat.\n"
        , "Your ClientId is <" <> expr clientId <> ">,\n"
        , "Your GroupId is <" <> expr groupId <> ">.\n"
        , "Type \"/quit\" when you quit.\n\n"
        ]

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
            str' <- clientGet cl

            let str = rstrip str' -- Chop newline
            logger $ "Client<" <> (expr clientId) <> "> entered raw strings: " <> expr str
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


--    _e :: Either SomeException ()
--        <- try $ race_ (broadcastReceiver broadcastCh) (race_ receiver server)
--    clientProcess srv cl


    -- Spawn 3 linked threads.
    race_ (broadcastReceiver broadcastCh) (race_ receiver server)
    -- Thread which accepts a request terminated here.
    return ()


handleMessage :: Server -> Group -> Client -> Message -> IO Bool
handleMessage Server{..} gr@Group{..} cl@Client{..} msg = do
    -- Send message to client
    output cl msg

    case msg of
        Command str -> do
            case words str of
                ["/quit"] -> do
                    clientPut cl $ "You left room.\n"
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
