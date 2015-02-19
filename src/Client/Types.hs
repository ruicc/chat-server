module Client.Types where

import App.Prelude
import Control.Concurrent.STM


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
