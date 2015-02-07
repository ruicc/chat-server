module Main where


import Network
import Control.Monad
import Control.Concurrent
import System.IO
import Text.Printf (printf)

main = withSocketsDo $ do
    let
        port = 3000 :: Int

    socket <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port
    forever $ do
        (handle, hostname, portnumber) <- accept socket
        printf "Accepted \n"
        forkFinally (echo handle) (\_ -> hClose handle)

echo h = forever $ do
    hSetBuffering h LineBuffering
    str <- hGetLine h
    hPutStrLn h str
