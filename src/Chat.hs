module Chat (runChatServer) where


import App.Prelude
import Network

import           Text.Printf (printf)

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


    let
        errorHandler :: Handle -> Either SomeException () -> IO ()
        errorHandler hdl (Left e) = do
            errorCollector server e
            hClose hdl
        errorHandler hdl _ = do
            hClose hdl

        -- QuitGame can be thrown anywhere, anytime.
        quitHandler :: QuitGame -> IO ()
        quitHandler QuitGame = tick server $ Log.ClientLeft

        run hdl = runClientThread server hdl `catch` quitHandler

    forever $ do
        (hdl, _hostname, _portnumber) <- accept socket
--        printf "Accepted from %s\n" hostname
        forkFinally (run hdl) (errorHandler hdl)
