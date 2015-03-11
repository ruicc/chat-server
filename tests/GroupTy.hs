module GroupTy where

import           Control.Applicative
import           Control.Monad.Trans.Cont (ContT(..))
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent.Object
import           Control.Concurrent.STM
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- | Group

newtype Group = Group { unGroup :: Object GMessage GReply }

type GroupId = Int
type ClientId = Int
type MemberName = String
type ClientName = String

data GroupData = GroupData
    { groupId :: GroupId
    , groupMembers :: Map.Map ClientId Client
    , groupState :: GroupState
    , groupSessionData :: GameSessionData
    }

data GroupState = Waiting | Preparing | Playing | Result | Deleted
    deriving (Eq, Show)

data Client = Client
    { clientName :: ClientName
    , clientConfig :: ClientConfig
    }
    deriving Show

data ClientConfig = ClientConfig
    { clientConfigColor :: Color
    }
    deriving Show

data Color = Red | Blue
    deriving Show

data GameSessionData = GameSessionData
    { gameStage :: GameStage
    }
    deriving Show

data GameStage = Morning | Night
    deriving Show

--------------------------------------------------------------------------------
-- Helper

initGroupData :: GroupId -> GroupData
initGroupData gid = GroupData
    { groupId = gid
    , groupMembers = Map.empty
    , groupState = Waiting
    , groupSessionData = GameSessionData
        { gameStage = Morning
        }
    }

newClient :: ClientName -> Color -> Client
newClient name cl = Client
    { clientName = name
    , clientConfig = ClientConfig
        { clientConfigColor = cl
        }
    }

--------------------------------------------------------------------------------

data GMessage
    -- Member CRUD
    = AddMember ClientId Client
    | RemoveMember ClientId
    | GetGroupId
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
    | GetGroupIdR GroupId
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

    new cl = Group <$> new cl
    (Group obj) ! msg = obj ! msg
    (Group obj) !? msg = obj !? msg
    kill (Group obj) = kill obj

instance ObjectLike (ContT r IO) Group where
    type OMessage Group = GMessage
    type OReply Group = GReply
    type OClass Group = Class GMessage GReply

    new cl = liftIO $ Group <$> new cl
    (Group obj) ! msg = liftIO $ obj ! msg
    (Group obj) !? msg = liftIO $ (liftIO <$> obj !? msg)
    kill (Group obj) = liftIO $ kill obj

--------------------------------------------------------------------------------
-- Group object instanciation

newGroup :: GroupId -> IO Group
newGroup gid = new Class
    { classInitializer = return $ initGroupData gid
    , classFinalizer = (\_st -> putStrLn "Cleanup")
    , classCallbackModule = CallbackModule $ \self@Self{..} msg -> do
        let
            getData = atomically $ readTVar selfState

            changeState from to = atomically $ do
                dat <- readTVar selfState
                case groupState dat of
                    from -> do
                        modifyTVar' selfState (\ dat -> dat { groupState = to })
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
            GetGroupId -> do
                dat <- getData
                return (GetGroupIdR $ groupId dat, self)
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
