module Main where

import Control.Applicative
import Control.Monad.Cont as Conc
import Control.Monad.State
import Control.Exception
import System.IO
import Data.Monoid

type Concurrent a = ContT () (StateT (a -> IO ()) IO) a

runConcurrent :: (a -> IO ()) -> Concurrent a -> IO ()
runConcurrent cont action = flip evalStateT cont $ runContT action (liftIO . cont)

--instance MonadState Concurrent (a -> IO ()) where

--main :: IO ()
--main = runConcurrent putStrLn $ do
--    liftIO $ getLine
--    path <- liftIO $ getLine
--    hdl <- liftIO $ openFile path ReadMode


main :: IO ()
main = runConcurrent putStrLn $ do
    path <- liftIO $ getLine
    hdl <- liftIO $ openFile path ReadMode
    exitStr <- callCC $ \ exit -> do
        str <- dosomething hdl `catchM` \ (e :: SomeException) -> do
            -- Cleanup
            liftIO $ hClose hdl
            liftIO $ putStrLn $ "Err: " <> show e
            liftIO $ putStrLn "Error occured, handle closed normally."
--            return "Exit!!"
            exit "Exit!!"
        liftIO $ putStrLn str
--        exit "Exit abnormally..."
        errorOccur str
        return "ExitCont??"
--        return $ str ++ "\n" ++ str
    liftIO $ putStrLn $ "Exit string: " <> exitStr

    return "String for cont"



-- Temporary, ignore exit continuation in Concurrent.
catchM :: Exception e => Concurrent a -> (e -> Concurrent a) -> Concurrent a
catchM action handler =
    ContT $ \cont -> do -- StateT
        StateT $ \s -> do -- IO
            runStateT (runContT action cont) s

            `catch` \e -> do
                runStateT (runContT (handler e) cont) s

--catchM :: Exception e => Concurrent a -> (e -> Concurrent a) -> Concurrent a
--catchM action handler =
--    ContT $ \cont -> do -- StateT
--        exit :: (a -> StateT (a -> IO ()) IO ()) <- get
--        StateT $ \s -> do -- IO
--            runStateT (runContT action cont) s
--
--            `catch` \e -> do
--                runStateT (runContT (handler e) exit) s
--catchM :: Exception e => Concurrent a -> (e -> Concurrent a) -> Concurrent a
--catchM action handler =
--    ContT $ \ cont -> StateT $ \ s ->
--        runConcurrent cont action `catch` \ err -> do
--            exit <- lift get
--            runConcurrent exit (handler err)

dosomething :: Handle -> Concurrent String
dosomething hdl = do
    liftIO $ hGetLine hdl

errorOccur :: String -> Concurrent String
errorOccur str = do
    liftIO $ throwIO $ ErrorCall "Heyhey, error Occured"
    liftIO $ putStrLn str
    return str

--wedge :: Concurrent a -> Concurrent a
--wedge action = do
--    oldCont <- lift get
--    res <- callCC $ \ exit -> do
--        lift $ put exit
--        action
--    lift $ put oldCont
--    return res


--forkFinally :: Concurrent a -> (Either SomeException a -> Concurrent ()) -> Concurrent ThreadId
--forkFinally action handler = ContT $ \cont -> runConcurrent cont action
