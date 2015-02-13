module Exception where

import App.Prelude
import Data.Typeable


data ClientException = KickedFromRoom
    deriving (Show, Typeable)

instance Exception ClientException

