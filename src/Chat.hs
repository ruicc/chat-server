module Chat (runChatServer) where


import App.Prelude
import Network

import           Text.Printf (printf)
import           System.IO as IO

import qualified Log as Log
import           Types
import           Server (runClientThread)


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


        let
            errHdlr :: Either SomeException () -> IO ()
            errHdlr (Left e) = do
                errorCollector server e
            errHdlr _ = return ()

        forkFinally (runClientThread server hdl) (\ e -> errHdlr e >> hClose hdl)
