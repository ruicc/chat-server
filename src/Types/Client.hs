module Types.Client where

import           Control.Applicative ((<$>))
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.ByteString.Short as SBS
import qualified Data.Unique as Uniq
import qualified System.IO as IO

import           Types.Message (Message)


type ClientId = Int
type ClientName = SBS.ShortByteString

data Client = Client
    { clientId :: ClientId
    , clientHandle :: IO.Handle
    , clientChan :: TChan Message -- ^ Mailbox sent to this client
    , clientThreadId :: ThreadId
    , clientConfig :: Maybe ClientConfig
    }
    deriving Show

data ClientConfig = ClientConfig
    { ccColor :: Color
    , ccName :: ClientName
    }
    deriving Show

data Color = Red | Blue
    deriving Show

instance Show (TChan a) where
    show tch = "{TChan: unknown}"

newClient
    :: IO.Handle
    -> IO Client -- ^ CSTM r???
                    -- ^    -> No. Client is not shared value.
                    -- ^    -> Client is shared within Server and Group, but CSTM r isn't required.
newClient hdl = do
    cid :: Int
        <- Uniq.hashUnique <$> Uniq.newUnique
    ch :: TChan Message
        <- newTChanIO
    tid <- myThreadId
    return $ Client
        { clientId = cid
        , clientHandle = hdl
        , clientChan = ch
        , clientThreadId = tid
        , clientConfig = Nothing
        }
