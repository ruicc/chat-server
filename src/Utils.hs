module Utils where

import           Data.List (intersperse)
import           Data.Char (isSpace)


rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse
