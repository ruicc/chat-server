module Main where

import App.Prelude
import Network
import Data.Maybe
import qualified Data.Aeson as A


main :: IO ()
main = do
    let
        threadNum = 1000
        loop tv = do
            threadDelay $ 1 * 1000 * 1000
            cnt <- atomically $ readTVar tv
            if cnt >= threadNum
                then return ()
                else loop tv

    counter <- newTVarIO 0
    forM_ [1..threadNum] $ \ i -> do
        forkIO $ clientProgram counter

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

    let
        getMessage :: IO Message
        getMessage = do
            sb <- rstrip <$> hGetLine hdl
            hFlush hdl
--            putStrLn $ "Rcv :: " <> sb -- logging
            return $ fromJust $ sbToMessage sb

        putSB :: ShortByteString -> IO ()
        putSB sb = do
            hPutStrLn hdl sb
            hFlush hdl
--            putStrLn $ "Snd >> " <> sb -- logging


    initMsg <- getMessage
    cl <- (\ (Init cid) -> newClient cid hdl) initMsg


    forM_ [1..100] $ \ i -> do
        groups <- getMessage
        putSB "/new alice's 2 20"

        join <- getMessage
        cl <- (\(Join gid) -> return cl { groupId = Just gid }) join

        putSB "Hello!"
        res <- getMessage

        putSB "/leave"
        leave <- getMessage

        return ()

    _gs <- getMessage

    putSB "/quit"

--    putStrLn $ "Log -- " <> (expr $ clientId cl)
--    putStrLn $ "Log -- OK"
    hClose hdl

    atomically $ modifyTVar' cnt succ




type ClientId = Int
type GroupId = Int

data Client = Client
    { clientId :: ClientId
    , clientHandle :: Handle
    , clientChan :: TChan Message
    , groupId :: Maybe GroupId
    }
data Message -- messages from server
    = Init ClientId
    | Groups [GroupId]
    | Join GroupId
    | Leave

newClient :: ClientId -> Handle -> IO Client
newClient cid hdl = do
    ch <- newTChanIO
    return $ Client cid hdl ch Nothing

receiver :: Client -> IO ()
receiver cl@Client{..} = do
    sb <- rstrip <$> hGetLine clientHandle
    hFlush clientHandle
    print sb -- logging
    case sbToMessage sb of
        Just msg -> atomically $ writeTChan clientChan msg
        Nothing -> return ()
    receiver cl

--client :: Client
   
sbToMessage :: ShortByteString -> Maybe Message
sbToMessage sb = case words sb of
    ["/init", cid'] -> case readInt cid' of
        Just (cid, _) -> Just $ Init cid
    ["/event", "join", gid'] -> case readInt gid' of
        Just (gid, _) -> Just $ Join gid
    ["/event", "leave"] -> Just $ Leave
    ("/groups" : gids') -> Just $ Groups $ map (fst . fromJust . readInt) gids' -- ひどいやつだ
    _ -> Nothing

