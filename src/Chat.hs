module Chat (runChatServer) where


import           App.Prelude
import           Network
import           Text.Printf (printf)

import qualified Log as Log
import           Types
import           Exception
import           Concurrent
import           Server (runClientThread)


runChatServer :: Int -> IO ()
runChatServer port = withSocketsDo $ do

    (erCh, stCh, logCh) <- Log.spawnCollectorThreads

    server <- newServer logCh stCh erCh

    socket <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port


    forever $ runConcurrent $ do -- Concurrent
        (hdl, hostname, _portnumber) <- liftIO $ accept socket
        logger server $ "Accepted from " <> expr hostname <> "\n" 

        forkFinally_ (run server hdl) (errorHandler server hdl)

run :: Server -> Handle -> Concurrent ()
run server hdl = runClientThread server hdl `catch_` quitHandler server

errorHandler :: Server -> Handle -> Either SomeException () -> Concurrent ()
errorHandler server hdl (Left e) = do
    errorCollector server e
    liftIO $ hClose hdl
errorHandler server hdl _ = do
    liftIO $ hClose hdl

-- QuitGame can be thrown anywhere, anytime.
quitHandler :: Server -> QuitGame -> Concurrent ()
quitHandler srv@Server{..} QuitGame = do
    logger srv "Client quit"
    tick srv Log.ClientLeft
