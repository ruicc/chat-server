module Main where

import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import qualified Control.Concurrent.Async as A
import qualified Control.Exception as E
import System.IO as IO
import qualified Data.IntMap as IM
import Network

import App.Prelude as P
import Concurrent
import Server
import Types
import Log
import Exception
import Utils

main :: IO ()
main = do
    let port = 3002
        host = "localhost"
        portId = PortNumber $ fromIntegral port
        clientNum = 100
        waitSec = 10
--    print =<< C.getNumCapabilities

    C.forkIO $ server portId
--    C.forkIO $ serverIO portId
    
    C.threadDelay $ 100 * 1000

    forM_ [1..clientNum] $ \n -> do
        spawnClient host portId n
        C.threadDelay $ 30

    C.threadDelay $ waitSec * 1000 * 1000


server portId = do

    (erCh, stCh, logCh) <- Log.spawnCollectorThreads
    srv <- newServer logCh stCh erCh
    socket <- listenOn portId

    forever $ runConcurrent $ do

        (hdl, hostname, _portnumber) <- lift $ accept socket

        (`forkFinally_` (\ _ -> lift $ IO.hClose hdl)) $ do
            lift $ IO.hSetBuffering hdl LineBuffering
            cl <- newClient hdl

            let
                name = "alice's"
                capacity = 2
                time = 200
                timeout = 20
            gr <- createGroupCIO srv name capacity time timeout

            forM_ [1..] $ \i -> do
                !() <- mask_ $ \restore -> do
                    !res <- joinGroup srv gr cl
                    let
                        finalizer = removeClient srv cl gr
                        clientErrorHandler = \ (_ :: ClientException) -> return ()
                        action = do
                            !() <- notifyClient srv gr cl
                            !() <- runClient srv gr cl
                            return ()
                    !() <- restore action `catch_` clientErrorHandler `finally_` finalizer
                    threadDelay $ 10 * 1000
                return ()


-- | Client connects to server, but it doesn't anything
spawnClient host portId n = do
    hdl <- connectTo host portId
    IO.hSetBuffering hdl LineBuffering

    (`C.forkFinally` \ _ -> IO.hClose hdl) $ forM_ [1..1000 :: Int] $ \ i -> do
        IO.hPutStrLn hdl $ "/leave"
--        IO.hPutStrLn hdl $ "Client " <> show n
        C.threadDelay $ 10 * 1000





------------------------------------------------------------------------------------------
-- | IO version

--serverIO portId = do
--
--    (erCh, stCh, logCh) <- Log.spawnCollectorThreads
--    srv <- newServer logCh stCh erCh
--    socket <- listenOn portId
--
--    forever $ do
--
--        (hdl, hostname, _portnumber) <- accept socket
--
--        (`C.forkFinally` (\ _ -> IO.hClose hdl)) $ do
--            IO.hSetBuffering hdl LineBuffering
--            cl <- runCIO return $ newClient hdl
--            gr <- S.atomically $ runCSTM return $ newGroup 1 "alice's" 2 3 4 5
--            runClientIO srv gr cl
--
--runClientIO :: Server -> Group -> Client -> IO ()
--runClientIO srv@Server{..} gr@Group{..} cl@Client{..} = do
--
--    !broadcastCh <- S.atomically $ S.dupTChan groupBroadcastChan
--
--    -- Spawn 3 linked threads.
--    A.race_
--        (Main.broadcastReceiver cl broadcastCh)
--        (A.race_ (Main.clientReceiver srv cl) (Main.clientServer srv gr cl))
----{-# NOINLINE runClient #-}
--
--
--broadcastReceiver :: Client -> TChan Message -> IO ()
--broadcastReceiver cl broadcastCh = forever $ do
--    S.atomically $ do
--        !msg
--            <- S.readTChan broadcastCh
--        Main.sendMessage cl msg
----    logger $ "BroadcastReceiver works"
--
--
--clientReceiver :: Server -> Client -> IO ()
--clientReceiver srv cl = forever $ do
--    !str <- runCIO return $ clientGet srv cl
----    logger $ "Client<" <> (expr clientId) <> "> entered raw strings: " <> expr str
--    S.atomically $ Main.sendMessage cl (Command str)
----{-# NOINLINE clientReceiver #-}
--
--
--clientServer :: Server -> Group -> Client -> IO ()
--clientServer srv gr cl@Client{..} = do
--
--    !msg
--        <- S.atomically $ S.readTChan clientChan
--    !continue <- Main.handleMessage srv gr cl msg
--    when continue
--            (Main.clientServer srv gr cl)
--    -- Left the room if continue == False.
----{-# NOINLINE clientServer #-}
--
--sendMessage :: Client -> Message -> S.STM ()
--sendMessage Client{..} msg = do
--    S.writeTChan clientChan msg
--
--handleMessage :: Server -> Group -> Client -> Message -> IO Bool
--handleMessage Server{..} gr@Group{..} cl@Client{..} msg = do
--    -- Send message to client
--    runConcurrent $ output cl msg
--
--    case msg of
--        Command str -> do
--            case words str of
--                ["/leave"] -> do
--                    -- Leave the room.
--                    return False
--
--                ["/quit"] -> do
--                    -- Quit the game.
--                    E.throwIO QuitGame
--
--                [] -> do
--                    -- Ignore empty messages.
--                    return True
--
--                _ -> do
----                    tick Log.GroupChat
--                    S.atomically $ runCSTM return $ sendBroadcast gr (Broadcast clientId str)
--                    return True
--        Broadcast _ _ -> do
--            return True
--
--        Notice _ -> do
--            return True
--
--        _ -> error "Not impl yet"
