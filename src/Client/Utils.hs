module Client.Utils where

import App.Prelude
import Data.Maybe
import Client.Types

getMessage :: Handle -> IO Message
getMessage hdl = do
    sb <- rstrip <$> hGetLine hdl
    hFlush hdl
#if DEVELOPMENT
    putStrLn $ "Rcv :: " <> sb -- logging
#endif
    return $ fromJust $ sbToMessage sb

putSB :: Handle -> ShortByteString -> IO ()
putSB hdl sb = do
    hPutStrLn hdl sb
    hFlush hdl
#if DEVELOPMENT
    putStrLn $ "Snd >> " <> sb -- logging
#endif

sbToMessage :: ShortByteString -> Maybe Message
sbToMessage sb = case words sb of
    ["!init", cid'] -> case readInt cid' of
        Just (cid, _) -> Just $ Init cid
    ["!event", "join", gid'] -> case readInt gid' of
        Just (gid, _) -> Just $ Join gid
    ["!event", "leave"] -> Just $ Leave
    ("!groups" : gids') -> Just $ Groups $ map (fst . fromJust . readInt) gids' -- FIXME: fromJust
    _ -> Nothing

