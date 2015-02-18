module Main where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Concurrent as Conc
import Control.Concurrent.STM as STM
import qualified Control.Exception as E
import System.IO
import Data.Monoid
import Concurrent

main :: IO ()
main = runConcurrent putStrLn $ do
    path <- liftIO $ getLine
    hdl <- liftIO $ openFile path ReadMode

    let
         errorhandler :: E.SomeException -> Concurrent String
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


    tv :: TVar Int <- liftIO $ newTVarIO 0

    fork $ do
        let
            loop = do
                wait $ 1 * 1000 * 1000
                runSTM $ modifyTVar' tv succ
                loop
        loop

    wait $ 3 * 1000 * 1000

    n <- runSTM $ readTVar tv

    return $ "fork & STM : " <> show n


dosomething :: Handle -> Concurrent String
dosomething hdl = do
    liftIO $ hGetLine hdl

errorOccur :: String -> Concurrent String
errorOccur str = do
    liftIO $ E.throwIO $ E.ErrorCall "Heyhey, error Occured"
    liftIO $ putStrLn str
    return str
