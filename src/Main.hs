module Main where

import           App.Prelude
import           App.Config
import           Chat (runChatServer)
--import           System.Remote.Monitoring

main :: IO ()
main = do

    cnf :: Config
        <- getConfig "config/settings.yml"

    let
        ekgCnf = ekgConfig cnf
        serverCnf = serverConfig cnf

    -- Ekg: access at http://localhost:3001 with browser or curl.
    -- curl -H "Accept: application/json" http://localhost:3001/
--    _ <- forkServer
--        (ekgHost ekgCnf)
--        (ekgPort ekgCnf)

    runChatServer (serverPort serverCnf)
