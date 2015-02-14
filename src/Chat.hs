module Chat (runChatServer) where


import App.Prelude
import Network

import           Text.Printf (printf)
import           System.IO as IO

import qualified Log as Log
import           Types
import           Exception
import           Server (runClientThread)


runChatServer :: Int -> IO ()
runChatServer port = withSocketsDo $ do

    (erCh, stCh, logCh) <- Log.spawnCollectorThreads

    server <- newServer logCh stCh erCh

    socket <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port

    forever $ do
        (hdl, hostname, _portnumber) <- accept socket
--        printf "Accepted from %s\n" hostname

        hSetBuffering hdl LineBuffering
--        hSetBuffering hdl NoBuffering


        let
            errHandler :: Either SomeException () -> IO ()
            errHandler (Left e) = do
                errorCollector server e
            errHandler _ = return ()

            -- QuitGame can be thrown anywhere, anytime.
            quitHandler :: QuitGame -> IO ()
            quitHandler QuitGame = tick server $ Log.ClientLeft

        forkFinally
                (runClientThread server hdl `catch` quitHandler)
                (\ e -> errHandler e >> hClose hdl)
