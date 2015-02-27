module Main where

import           Data.Monoid
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import qualified Control.Concurrent.Async as A
import qualified Control.Exception as E
import           System.IO as IO
import qualified Data.IntMap as IM
import           Control.Concurrent.Structured
import           Network

import           App.Prelude as P
import           App.Config
import           Server
import           Types
import           Log
import           Exception
import           Utils

main :: IO ()
main = do
    cnf :: Config
        <- getConfig "config/settings.yml"

    let port = serverPort $ serverConfig cnf
        host = serverHost $ serverConfig cnf
        portId = PortNumber $ fromIntegral port
        clientNum = clientSpawnThreads $ clientConfig cnf
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
