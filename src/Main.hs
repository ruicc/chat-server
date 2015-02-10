module Main where

import           Chat (runChatServer)

port :: Int
port = 3000

main :: IO ()
main = runChatServer port
