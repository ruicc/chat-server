module Client.Actions where

import App.Prelude
import Client.Utils
import Client.Types


initialize :: Handle -> IO Client
initialize hdl = do
    initMsg <- getMessage hdl
    case initMsg of
        (Init cid) -> newClient cid hdl
        _ -> error "init error"

createNewGroup :: Client -> ShortByteString -> Int -> Int -> IO Client
createNewGroup cl@Client{..} name capacity timeout = do
    groups <- getMessage clientHandle
    putSB clientHandle $ "/new " <> name <> " " <> expr capacity <> " " <> expr timeout

    join <- getMessage clientHandle
    cl <- case join of
        (Join gid) -> return cl { groupId = Just gid }
        _ -> error "join error"

    return cl

chat :: Client -> ShortByteString -> IO ()
chat Client{..} sb = do
    putSB clientHandle sb
    _res <- getMessage clientHandle
    return ()

leaveGroup :: Client -> IO Client
leaveGroup cl@Client{..} = do
    putSB clientHandle "/leave"
    _leave <- getMessage clientHandle
    return cl { groupId = Nothing }

quit :: Client -> IO ()
quit Client{..} = do
    getMessage clientHandle
    putSB clientHandle "/quit"
