module Exception where

import App.Prelude
import Data.Typeable


data ClientException = KickedFromRoom
    deriving (Show, Typeable)

data QuitGame = QuitGame
    deriving (Show, Typeable)

instance Exception ClientException
instance Exception QuitGame
