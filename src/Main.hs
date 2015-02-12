module Main where

import           App.Prelude
import           Chat (runChatServer)
import           System.Remote.Monitoring

port :: Int
port = 3000

main :: IO ()
main = do
    -- ekg: access at http://localhost:3001 with browser or curl.
    -- curl -H "Accept: application/json" http://localhost:3001/
    _ <- forkServer "localhost" 3001

    runChatServer port
