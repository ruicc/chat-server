module App.Types.ServerMessage where

import           App.Prelude

import           Data.Aeson (encode, decode)
import           Data.Aeson.TH
import           Data.Char (toLower)
import qualified Data.Text as T



data ServerMessage
    = Init
        { smClientId :: Int
        }
    | AllRooms
        { smRooms :: [Rooms]
        }
    | Chat
        { smMessage :: T.Text
        , smClientId :: Int
        , smTimestamp :: Int
        }
    | Play
    deriving (Show, Read)

deriveJSON defaultOptions ''Color
deriveJSON defaultOptions
    { fieldLabelModifier = \ name' ->
        -- Change to smallCamelCase.
        let name = drop 2 name'
        in map toLower (take 1 name) <> drop 1 name
    , omitNothingFields = True
    }
    ''ServerMessage
