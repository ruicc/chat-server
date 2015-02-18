module Main where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
--import qualified Control.Concurrent as Conc
import Control.Concurrent.STM as STM
import qualified Control.Exception as E
import System.IO
import Data.Monoid
import Concurrent2

main :: IO ()
main = runCIO return $ do
    path <- liftIO $ getLine
    hdl <- liftIO $ openFile path ReadMode

    let
         errorhandler :: SomeException -> Concurrent String
         errorhandler e = do
             liftIO $ hClose hdl
             liftIO $ putStrLn $ "Err: " <> show e
             liftIO $ putStrLn "Error occured, handle closed normally."
             return "Exit!!"

    exitStr <- handle errorhandler $ do
        str <- dosomething hdl
        str1 <- dosomething hdl
        str2 <- dosomething hdl `onException` (liftIO $ putStrLn "Log: onException 2")

        liftIO $ putStrLn str
        liftIO $ putStrLn str1
        liftIO $ putStrLn str2
        errorOccur str `onException` (liftIO $ putStrLn "Log: onException 1")
        return "ExitCont??"
    liftIO $ putStrLn $ "Exit string: " <> exitStr


--    tv :: TVar Int <- liftIO $ newTVarIO 0
--
--    forkC $ do
--        let
--            loop = do
--                threadDelay $ 100 * 1000
--                runSTM $ modifyTVar' tv succ
--                loop
--        loop
--
--    threadDelay $ 1 * 1000 * 1000
--
--    n <- runSTM $ readTVar tv

    -- error?
--    joinC $ runSTM $ readPrint tv
--
--    return $ "fork & STM : " <> show n



dosomething :: Handle -> Concurrent String
dosomething hdl = do
    liftIO $ hGetLine hdl

errorOccur :: String -> Concurrent String
errorOccur str = do
    throwC $ E.ErrorCall "Heyhey, error Occured"
    liftIO $ putStrLn str
    return str


--readPrint :: TVar Int -> STM (Concurrent ())
--readPrint tv = do
--    n <- readTVar tv
--    return $ liftIO $ do
--        print n
