module Main where

import App.Prelude
import Client.Types
import Client.Actions
import Client.Utils
import Network
import Data.Maybe
import qualified Data.Aeson as A


main :: IO ()
main = do
    let
        threadNum = 2000
        loop tv = do
            threadDelay $ 1 * 1000 * 1000
            cnt <- atomically $ readTVar tv
            if cnt >= threadNum
                then return ()
                else loop tv

    counter <- newTVarIO 0
    forM_ [1..threadNum] $ \ i -> do
        forkIO $ clientProgram counter
        threadDelay $ 100

    loop counter


clientProgram :: TVar Int -> IO ()
clientProgram cnt = do
    let
        port :: Int
        port = 3000

        portId :: PortID
        portId = PortNumber $ fromIntegral port

    hdl <- connectTo "localhost" portId
    hSetBuffering hdl LineBuffering

    threadDelay $ 100 * 1000

    cl <- initialize hdl

    forM_ [1..20] $ \ i -> do
        cl' <- createNewGroup cl "alice's" 2 20

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
   
