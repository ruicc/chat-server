module Types.Message
    ( Message(..), ClientMessage(..)
    ) where

import           Prelude as P
import qualified Data.ByteString.Short as SBS


type Short = SBS.ShortByteString
type ClientId = Int
type GroupId = Int
type GroupName = Short
type GroupCapacity = Int
type PlayTime = Int
type Timeout = Int

data Message
    = Notice Short
    | Tell ClientId Short
    | Broadcast ClientId Short
    | Command Short
    deriving Show

data ClientMessage
    = Quit
    | NewGroup GroupName GroupCapacity PlayTime Timeout
    | JoinGroup GroupId
    deriving Show
