module Main where


import Prelude hiding (log, lookup)
import Network

import Control.Monad
import Control.Concurrent

import           Text.Printf (printf)

import           System.IO as IO

import qualified Log as Log
import           Types
import           Client (clientProcess)

main :: IO ()
main = withSocketsDo $ do
    let
        port = 3000 :: Int

    logCh <- Log.spawnLogger
    server <- newServer logCh

    socket <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port

    forever $ do
        (hdl, hostname, portnumber) <- accept socket
        printf "Accepted from %s\n" hostname

        hSetBuffering hdl LineBuffering
--        hSetBuffering hdl NoBuffering

        cl <- newClient hdl

        forkFinally (clientProcess server cl) (\ _ -> hClose hdl)

