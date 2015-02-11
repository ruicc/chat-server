module Chat (runChatServer) where


import Prelude hiding (log, lookup)
import Network

import Control.Monad
import Control.Concurrent
import Control.Exception

import           Data.Monoid
import           Text.Printf (printf)
import           System.IO as IO

import qualified Log as Log
import           Types
import           Client (clientProcess)


runChatServer :: Int -> IO ()
runChatServer port = withSocketsDo $ do

    (erCh, stCh, logCh) <- Log.spawnCollectorThreads

    server <- newServer logCh stCh erCh

    socket <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port

    forever $ do
        (hdl, hostname, _portnumber) <- accept socket
        printf "Accepted from %s\n" hostname

        hSetBuffering hdl LineBuffering
--        hSetBuffering hdl NoBuffering

        cl <- newClient hdl
        tick server $ Log.ClientNew

        let
            errHdlr :: Either SomeException () -> IO ()
            errHdlr (Left e) = do
                putStrLn $ "error: " <> show e
                errorCollector server e
            errHdlr _ = return ()

        forkFinally (clientProcess server cl) (\ e -> errHdlr e >> hClose hdl)
