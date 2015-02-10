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


--    forkIO $ runChatServer port
--    threadDelay $ 500 * 1000


--    hGetLine hdl
--    hGetLine hdl
    forM_ [1..20] $ \j -> do
        forkIO $ do
            hdl <- connectTo "localhost" portId
            hSetBuffering hdl LineBuffering
            threadDelay $ 100 * 1000


            forM_ [1..50] $ \i -> do
--                hPutStrLn hdl "/new"
--                threadDelay $ 1000
                hPutStrLn hdl "2"
                threadDelay $ 2000
                hPutStrLn hdl $ "Hello " ++ show j ++ " " ++ show i
                threadDelay $ 2000
                hPutStrLn hdl "/quit"
                threadDelay $ 5000
            hPutStrLn hdl "/quit"

--            hClose hdl

    threadDelay $ 4 * 1000 * 1000
    -- TODO ...

--    defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [ qcProps ]

qcProps = testGroup "QuickCheck"
    [ QC.testProperty "id == reverse . reverse" $
        \ (list :: [Int]) -> list == reverse (reverse list)
    ]
