module Client.Utils where

import App.Prelude
import Data.Maybe
import Client.Types

getMessage :: Handle -> IO (Maybe Message)
getMessage hdl = do
    sb <- rstrip <$> hGetLine hdl
    hFlush hdl
#if DEVELOPMENT
    putStrLn $ "Rcv :: " <> sb -- logging
#endif
    return $ sbToMessage sb

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
        Nothing -> Nothing
    ["!event", "join", gid'] -> case readInt gid' of
        Just (gid, _) -> Just $ Join gid
        Nothing -> Nothing
    ["!event", "leave"] -> Just $ Leave
    ("!groups" : gids') -> Just $ Groups
        (concat $ map (\case
            Just x -> [x]
            Nothing -> []
        )
        $ map (\bs -> fmap fst $ readInt bs)
        gids')
    _ -> Nothing
  where
    isJust (Just _) = True
    isJust Nothing = False

