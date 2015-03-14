module Types
    ( module Types.Server
    , module Types.Group
    , module Types.Client
    , module Types.Message
    , registerGroup, removeGroup, getGroup, getAllGroups
    , sendMessage, sendBroadcast
    , addClient, removeClient
    , output, clientPut, clientGet
    , newUniqueInt
    ) where

import           App.Prelude as P

--import qualified Data.Map as Map
import qualified Data.IntMap as IM
import qualified Data.Unique as Uniq
import qualified Control.Concurrent as Conc
import           Control.Concurrent.Structured
import           Control.Concurrent.Structured.Object

import           Types.Server
import           Types.Group
import           Types.Client
import           Types.Message
import           Log

------------------------------------------------------------------------------------------
-- | Server and Group

registerGroup :: Server -> Group -> CSTM r ()
registerGroup srv gr =
    modifyTVar'
        (serverGroups srv)
        (\ groups -> IM.insert (groupId gr) gr groups)

removeGroup :: Server -> Group -> CSTM r ()
removeGroup srv gr =
    modifyTVar'
        (serverGroups srv)
        (\ groups -> IM.delete (groupId gr) groups)

getGroup :: Server -> GroupId -> CSTM r (Maybe Group)
getGroup srv gid = do
    groups <- readTVar (serverGroups srv)
    return $ IM.lookup gid groups

getAllGroups :: Server -> CSTM r [(GroupId, Group)]
getAllGroups srv = do
    groups <- readTVar (serverGroups srv)
    return $ IM.toList groups

------------------------------------------------------------------------------------------
-- | Group and Client

addClient :: Server -> Group -> Client -> Concurrent Bool
addClient srv gr cl@Client{..} = do
    AddMemberR b <- join (gr !? AddMember clientId cl)
    tick srv Log.GroupJoin
    return b

removeClient :: Server -> Group -> Client -> Concurrent ()
removeClient srv gr cl@Client{..} = do
    join (gr !? RemoveMember clientId)
    tick srv Log.GroupLeft


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

clientPut :: Server -> Client -> ShortByteString -> Concurrent ()
clientPut srv cl str = liftIO $ hPutStrLn (clientHandle cl) str

clientGet :: Server -> Client -> Concurrent ShortByteString
clientGet srv cl = liftIO $ hGetLine (clientHandle cl)
