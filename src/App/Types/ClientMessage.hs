module App.Types.ClientMessage where

import           App.Prelude

import           Data.Aeson (encode, decode)
import           Data.Aeson.TH
import           Data.Char (toLower)
import qualified Data.Text as T



data ClientMessage
    = Init
        { cmInitOK :: Bool
        }
    | CreateRoom
        { cmRoomName :: T.Text
        , cmRootCapacity :: Int
        , cmTimeout :: Int -- ^ Seconds to timeout
        }
    | JoinRoom
        { cmRoomId :: Int
        }
    | UserConfig
        { cmColor :: Color
        , cmScreenName :: T.Text
        }
    | Play -- ^ To control air plain
        { cmDirection :: Int
        , cmDiffHitPoints :: Int
        }
    | Died -- ^ To notify server of player's death
    | Chat
        { cmMessage :: T.Text
        , cmClientId :: Int
        }
    | ExitPlay -- ^ Exit button on playing screen.
    | Quit -- ^ Player gone away from Game.
    deriving (Show, Read)

data Color = Red | Green | Blue | Black
    deriving (Show, Read)

deriveJSON defaultOptions ''Color
deriveJSON defaultOptions
    { fieldLabelModifier = \ name' ->
        -- Change to smallCamelCase.
        let name = drop 2 name'
        in map toLower (take 1 name) <> drop 1 name
    , omitNothingFields = True
    }
    ''ClientMessage
