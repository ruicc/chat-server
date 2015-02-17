module Main where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Concurrent as Conc
import Control.Exception
import System.IO
import Data.Monoid

--type Concurrent r a = ContT r (StateT (a -> IO r) IO) a
type Concurrent a = ContT () (StateT (a -> IO ()) IO) a
--type Concurrent a = ContT () IO a

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

    let
         errorhandler :: SomeException -> Concurrent String
         errorhandler e = do
             liftIO $ hClose hdl
             liftIO $ putStrLn $ "Err: " <> show e
             liftIO $ putStrLn "Error occured, handle closed normally."
             return "Exit!!"

    exitStr <- handleM errorhandler $ do
        str <- dosomething hdl
        str1 <- dosomething hdl
        str2 <- dosomething hdl

--    exitStr <- callCC $ \ exit -> do
--        str <- dosomething hdl `catchM` \ (e :: SomeException) -> do
--            -- Cleanup
--            liftIO $ hClose hdl
--            liftIO $ putStrLn $ "Err: " <> show e
--            liftIO $ putStrLn "Error occured, handle closed normally."
----            return "Exit!!"
--            exit "Exit!!"
        liftIO $ putStrLn str
        liftIO $ putStrLn str1
        liftIO $ putStrLn str2
        errorOccur str
        return "ExitCont??"
--        return $ str ++ "\n" ++ str
    liftIO $ putStrLn $ "Exit string: " <> exitStr

    return "String for cont"



catchM :: Exception e => Concurrent a -> (e -> Concurrent a) -> Concurrent a
catchM action handler =
    callCC $ \exit -> do
        ContT $ \cont -> do -- StateT
            StateT $ \s -> do -- IO
                runStateT (runContT action cont) s
                    `catch` \e -> runStateT (runContT (handler e) (\a -> runContT (exit a) return)) s

handleM :: Exception e => (e -> Concurrent a) -> Concurrent a -> Concurrent a
handleM = flip catchM

--onExceptionM :: Concurrent a -> Concurrent b -> Concurrent a
--onExceptionM action handler = action `catchM` \ (e :: SomeException) -> do
--    _ <- handler
--    liftIO $ throwIO e

--catchM :: Exception e => Concurrent a -> (e -> Concurrent a) -> Concurrent a
--catchM action handler =
--    ContT $ \cont -> do -- StateT
--        exit :: (a -> StateT (a -> IO ()) IO ()) <- get -- ???
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

--forkFinally :: Concurrent a a -> (Either SomeException a -> Concurrent () ()) -> Concurrent () ThreadId
--forkFinally action handler = ContT $ \cont -> do -- StateT
--    tid <- StateT $ \exit -> do -- IO
--        tid <- Conc.forkFinally
--                (evalStateT (runContT action (return)) exit)
----                (\e -> handler e)
--                (\e -> void $ runStateT (runContT (handler e) (liftIO . exit)) exit) -- ???
--        return (tid, exit)
--    cont tid
