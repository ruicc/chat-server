module Chat (runChatServer) where


import Prelude hiding (log, lookup)
import Network

import Control.Monad
import Control.Concurrent

import           Text.Printf (printf)

import           System.IO as IO

import qualified Log as Log
import           Types
import           Client (clientProcess)


runChatServer :: Int -> IO ()
runChatServer port = withSocketsDo $ do

    statCh <- Log.spawnAggregator
    logCh <- Log.spawnLogger
    server <- newServer logCh statCh

    socket <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port

    forever $ do
        (hdl, hostname, _portnumber) <- accept socket
        printf "Accepted from %s\n" hostname

        hSetBuffering hdl LineBuffering
--        hSetBuffering hdl NoBuffering

        cl <- newClient hdl
        tick server $ Log.ClientNew

        forkFinally (clientProcess server cl) (\ _ -> hClose hdl)
