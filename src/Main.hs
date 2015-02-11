module Main where

import           App.Prelude
import           Chat (runChatServer)

port :: Int
port = 3000

main :: IO ()
main = runChatServer port
