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

createNewGroup :: Client -> ShortByteString -> Int -> Int -> Int -> IO (Client, GroupId)
createNewGroup cl@Client{..} name capacity time timeout = do
    groups <- getMessage clientHandle
    putSB clientHandle $ "/new " <> name <> " " <> expr capacity <> " " <> expr time <> " " <> expr timeout

    join <- getMessage clientHandle
    cl <- case join of
        (Join gid) -> return (cl { groupId = Just gid }, gid)
        _ -> error "join error"

    return cl

chat :: Client -> ShortByteString -> IO ()
chat Client{..} sb = do
    putSB clientHandle sb
    _res <- getMessage clientHandle
    return ()

joinGroup :: Client -> GroupId -> IO Client
joinGroup cl@Client{..} gid = do
    putSB clientHandle $ "/join " <> (expr $ gid)
    _join <- getMessage clientHandle
    return cl { groupId = Just gid }

leaveGroup :: Client -> IO Client
leaveGroup cl@Client{..} = do
    putSB clientHandle "/leave"
    _leave <- getMessage clientHandle
    return cl { groupId = Nothing }

quit :: Client -> IO ()
quit Client{..} = do
    getMessage clientHandle
    putSB clientHandle "/quit"
