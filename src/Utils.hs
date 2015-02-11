module Utils where

import           App.Prelude
import           Data.Monoid
import           Data.Char (isSpace)


rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse
