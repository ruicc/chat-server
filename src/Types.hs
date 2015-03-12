module Types
    ( module Types.Group
    , module Types.Client
    , module Types.Message
    , sendMessage, sendBroadcast
    , output
    , newUniqueInt
    ) where

import           App.Prelude as P

import qualified Data.Map as Map
import qualified Data.IntMap as IM
import qualified Data.Unique as Uniq
import qualified Control.Concurrent as Conc
import           Control.Concurrent.Structured

import           Types.Group
import           Types.Client
import           Types.Message


------------------------------------------------------------------------------------------
-- | Message

sendMessage :: Client -> Message -> CSTM r ()
sendMessage Client{..} msg = writeTChan clientChan msg


sendBroadcast :: Group -> Message -> CSTM r ()
sendBroadcast gr@Group{..} msg = do
--    addHistory gr msg
    writeTChan groupBroadcastChan msg


--addHistory :: Group -> Message -> CSTM r ()
--addHistory Group{..} msg = do
--    hist <- readTVar groupHistory
--    if length hist >= 20
--        -- Keep only latest 20 messages
--        then writeTVar groupHistory $ msg : take 19 hist
--        else writeTVar groupHistory $ msg : hist
--
--getHistory :: Group -> CSTM r [Message]
--getHistory Group{..} = readTVar groupHistory


output :: Client -> Message -> CIO r ()
output Client{..} msg = do
    let
        out' (Command _) = return ()
        out' (Broadcast cid str) = hPutStrLn clientHandle $ "Client<" <> expr cid <> "> : " <> str
        out' (Notice str) = hPutStrLn clientHandle $ str
        out' _ = error "Not impl yet"
    liftIO $ out' msg

newUniqueInt :: IO Int
newUniqueInt = Uniq.hashUnique <$> Uniq.newUnique
