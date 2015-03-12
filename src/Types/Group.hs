module Types.Group where

import           Control.Applicative
import           Control.Monad.Trans.Cont (ContT(..))
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent.Object
import           Control.Concurrent.STM
import qualified Data.Map as Map
import qualified Data.ByteString.Short as SBS

import           Types.Client
import           Types.Message

--------------------------------------------------------------------------------
-- | Group

data Group = Group
    { groupValue :: GroupValue

    -- TODO: Move to Group Data to hide.
    , groupBroadcastChan :: TChan Message -- ^ Write Only channel for group broadcast
--    , groupHistory :: TVar [Message]

    -- Front-end of group thread
    , groupObject :: Object GMessage GReply
    }

type GroupId = Int
--type MemberName = String

--type GroupName = String
type GroupName = SBS.ShortByteString
type Timestamp = Int
type GroupCapacity = Int
type PlayTime = Int
type Timeout = Int

-- | Immutable values about Group.
data GroupValue = GroupValue
    { gvGroupId :: GroupId
    , gvGroupName :: GroupName
    , gvGroupCapacity :: GroupCapacity
    , gvGroupCreatedAt :: Timestamp -- ^ UnixTime
    , gvGroupTimeout :: Timeout -- ^ Wait time (sec)
    }

-- | Getters
groupId        = gvGroupId . groupValue
groupName      = gvGroupName . groupValue
groupCapacity  = gvGroupCapacity . groupValue
groupCreatedAt = gvGroupCreatedAt . groupValue
groupTimeout   = gvGroupTimeout . groupValue

-- | Mutable variables hidden behind Object.
data GroupMutable = GroupMutable
    { groupMembers :: Map.Map ClientId Client
    , groupMemberCount :: Int
    , groupControlState :: GroupControlState
    , groupSessionData :: GameSessionData
    }

data GroupControlState = Waiting | Preparing | Playing | Result | Deleted
    deriving (Eq, Show)

data GameSessionData = GameSessionData
    { gameStage :: GameStage
    , gamePlayTime :: PlayTime
    }
    deriving Show

data GameStage = Morning | Night
    deriving Show

--------------------------------------------------------------------------------
-- Helper

initGroupMutable :: GameStage -> PlayTime -> GroupMutable
initGroupMutable stage playtime = GroupMutable
    { groupMembers = Map.empty
    , groupMemberCount = 0
    , groupControlState = Waiting
    , groupSessionData = GameSessionData
        { gameStage = stage
        , gamePlayTime = playtime
        }
    }

--------------------------------------------------------------------------------
-- TMP data...

--type ClientId = Int
--type ClientName = String
--
--data Client = Client
--    { clientName :: ClientName
--    , clientConfig :: ClientConfig
--    }
--    deriving Show
--
--data ClientConfig = ClientConfig
--    { clientConfigColor :: Color
--    }
--    deriving Show
--
--data Color = Red | Blue
--    deriving Show
--
--newClient :: ClientName -> Color -> Client
--newClient name cl = Client
--    { clientName = name
--    , clientConfig = ClientConfig
--        { clientConfigColor = cl
--        }
--    }


--------------------------------------------------------------------------------

data GMessage
    -- Member CRUD
    = AddMember ClientId Client
    | RemoveMember ClientId
    | GetMember ClientId
    | GetAllMembers
    -- Change state
    | ToPreparing
    | ToPlaying
    | ToResult
    -- Session Data
    | GetSessionData
    | PutSessionData GameSessionData

data GReply
    = AddMemberR Bool
    | RemoveMemberR Bool
    | GetMemberR (Maybe Client)
    | GetAllMembersR [ClientId]
    | ToPreparingR Bool
    | ToPlayingR Bool
    | ToResultR Bool
    | GetSessionDataR GameSessionData
    | PutSessionDataR
    deriving (Show)

--------------------------------------------------------------------------------

instance ObjectLike IO Group where
    type OMessage Group = GMessage
    type OReply Group = GReply
    type OClass Group = Class GMessage GReply

--    new cl = Group <$> new cl
    gr ! msg = groupObject gr ! msg
    gr !? msg = groupObject gr !? msg
    kill gr = kill $ groupObject gr

instance ObjectLike (ContT r IO) Group where
    type OMessage Group = GMessage
    type OReply Group = GReply
    type OClass Group = Class GMessage GReply

--    new cl = liftIO $ Group <$> new cl
    gr ! msg = liftIO $ groupObject gr ! msg
    gr !? msg = liftIO $ (liftIO <$> groupObject gr !? msg)
    kill gr = liftIO $ kill $ groupObject gr

--------------------------------------------------------------------------------
-- Group object instanciation

newGroup
    :: GroupValue
    -> IO Group
newGroup gVal = do
    obj <- new $ groupClass
    ch <- atomically newBroadcastTChan
    return $ Group
        { groupValue = gVal
        , groupBroadcastChan = ch
        , groupObject = obj
        }

groupClass :: Class GMessage GReply GroupMutable
groupClass =
    let
        stage = Morning
        playtime = 120
    in
        Class
            { classInitializer = return $ initGroupMutable stage playtime
            , classFinalizer = (\_st -> putStrLn "Cleanup")
            , classCallbackModule = CallbackModule $ \self@Self{..} msg -> do
                let
                    getData = atomically $ readTVar selfState

                    changeState from to = atomically $ do
                        dat <- readTVar selfState
                        case groupControlState dat of
                            from -> do
                                modifyTVar' selfState (\ dat -> dat { groupControlState = to })
                                return True
                            _ -> return False

                case msg of
                    AddMember mid client -> do
                        atomically $ modifyTVar' selfState
                                (\ dat -> dat { groupMembers = Map.insert mid client (groupMembers dat) })
                        return (AddMemberR True, self)
                    RemoveMember mid -> do
                        atomically $ modifyTVar' selfState
                                (\ dat -> dat { groupMembers = Map.delete mid (groupMembers dat) })
                        return (RemoveMemberR True, self)
                    GetMember mid -> do
                        dat <- getData
                        return (GetMemberR $ Map.lookup mid (groupMembers dat), self)
                    GetAllMembers -> do
                        mids :: [ClientId]
                            <- (map fst . Map.toList . groupMembers) <$> getData
                        return (GetAllMembersR mids, self)

                    ToPreparing -> do
                        b <- changeState Waiting Preparing
                        return (ToPreparingR b, self)
                    ToPlaying -> do
                        b <- changeState Preparing Playing
                        return (ToPlayingR b, self)
                    ToResult -> do
                        b <- changeState Playing Result
                        return (ToResultR b, self)

                    GetSessionData -> do
                        dat <- getData
                        return (GetSessionDataR (groupSessionData dat), self)
                    PutSessionData sessDat -> do
                        atomically $ modifyTVar' selfState
                                (\ dat -> dat { groupSessionData = sessDat })
                        return (PutSessionDataR, self)
            }