module Exception where

import App.Prelude
import Data.Typeable
import Control.Exception


data ClientException
    = KickedFromRoom
    | LeaveRoom
    deriving (Show, Typeable)

data QuitGame = QuitGame
    deriving (Show, Typeable)

instance Exception ClientException
instance Exception QuitGame
