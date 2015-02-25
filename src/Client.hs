module Main where

import App.Prelude
import App.Config
import Control.Concurrent
import Control.Concurrent.STM
import Client.Types
import Client.Actions
import Client.Utils
import Network
import Data.Maybe (fromJust)


main :: IO ()
main = do

    cnf <- getConfig "config/settings.yml"

    let
        clientCnf = clientConfig cnf

        threadNum = clientSpawnThreads clientCnf
        loop tv = do
            threadDelay $ 1 * 1000 * 1000
            cnt <- atomically $ readTVar tv
            if cnt >= threadNum
                then return ()
                else loop tv

    counter <- newTVarIO 0
    forM_ [1..threadNum] $ \ i -> do
        void $ forkIO $ clientProgram cnf counter
        threadDelay $ 100

    loop counter


clientProgram :: Config -> TVar Int -> IO ()
clientProgram cnf cnt = do
    let
        serverCnf = serverConfig cnf

        port :: Int
        port = serverPort serverCnf

        host :: String
        host = serverHost serverCnf

        portId :: PortID
        portId = PortNumber $ fromIntegral port

    hdl <- connectTo host portId
    hSetBuffering hdl LineBuffering

    threadDelay $ 100 * 1000

    cl <- initialize hdl

    forM_ [1..20 :: Int] $ \ i -> do
        (cl', gid) <- createNewGroup cl "alice's" 2 20 30

        chat cl' "Hello!"

        cl <- leaveGroup cl'

        return ()

    quit cl

--    putStrLn $ "Log -- " <> (expr $ clientId cl)
--    putStrLn $ "Log -- OK"
    hClose hdl

    atomically $ modifyTVar' cnt succ





receiver :: Client -> IO ()
receiver cl@Client{..} = do
    sb <- rstrip <$> hGetLine clientHandle
    hFlush clientHandle
    print sb -- logging
    case sbToMessage sb of
        Just msg -> atomically $ writeTChan clientChan msg
        Nothing -> return ()
    receiver cl
