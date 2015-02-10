module Main where

import Test.Tasty
--import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import System.Exit (exitFailure, exitSuccess)

import           Control.Monad
import           Control.Concurrent
import           Network
import           System.IO

import           Chat (runChatServer)
import           Client (clientProcess)
import           Types
import           Log as Log


main :: IO ()
main = do
    let
        port :: Int
        port = 3000

        portId :: PortID
        portId = PortNumber $ fromIntegral port


    forkIO $ runChatServer port

    threadDelay $ 500 * 1000

    hdl <- connectTo "localhost" portId
    hSetBuffering hdl LineBuffering

    hGetLine hdl
    hGetLine hdl
    hPutStrLn hdl "/new"

    -- TODO ...

--    defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [ qcProps ]

qcProps = testGroup "QuickCheck"
    [ QC.testProperty "id == reverse . reverse" $
        \ (list :: [Int]) -> list == reverse (reverse list)
    ]
