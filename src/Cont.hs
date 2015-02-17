module Main where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Concurrent as Conc
import Control.Exception
import System.IO
import Data.Monoid

type Concurrent a = ContT () IO a

runConcurrent :: (a -> IO ()) -> Concurrent a -> IO ()
runConcurrent cont action = runContT action (liftIO . cont)


main :: IO ()
main = runConcurrent putStrLn $ do
    let
        loop = do
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
                str2 <- dosomething hdl `onExceptionM` (liftIO $ putStrLn "Log: onExceptionM 2")

                liftIO $ putStrLn str
                liftIO $ putStrLn str1
                liftIO $ putStrLn str2
                errorOccur str `onExceptionM` (liftIO $ putStrLn "Log: onExceptionM 1")
                return "ExitCont??"
            liftIO $ putStrLn $ "Exit string: " <> exitStr

            return "String for cont"

            loop
    loop


dosomething :: Handle -> Concurrent String
dosomething hdl = do
    liftIO $ hGetLine hdl

errorOccur :: String -> Concurrent String
errorOccur str = do
    liftIO $ throwIO $ ErrorCall "Heyhey, error Occured"
    liftIO $ putStrLn str
    return str

------------------------------------------------------------------------------------------

catchM :: Exception e => Concurrent a -> (e -> Concurrent a) -> Concurrent a
catchM action handler =
    callCC $ \exit -> do
        ContT $ \cont -> runContT action cont
                    `catch` \e -> runContT (handler e) (\a -> runContT (exit a) return)

handleM :: Exception e => (e -> Concurrent a) -> Concurrent a -> Concurrent a
handleM = flip catchM

onExceptionM :: Concurrent a -> Concurrent b -> Concurrent a
onExceptionM action handler =
    action `catchM` \ (e :: SomeException) -> do
        _ <- handler
        liftIO $ throwIO e

forkFinally :: Concurrent a -> (Either SomeException a -> Concurrent ()) -> Concurrent ThreadId
forkFinally = undefined


--forkFinally :: Concurrent a -> (Either SomeException a -> Concurrent ()) -> Concurrent ThreadId
--forkFinally action handler = ContT $ \cont -> do -- IO
--    void $ Conc.forkFinally
--                (runContT action cont)
--                (\e ->  runContT (handler e) cont)
--                (\e ->  runContT (handler e) (liftIO . exit))
--    cont tid

--forkFinally action handler = ContT $ \cont -> do -- StateT
--    tid <- StateT $ \exit -> do -- IO
--        tid <- Conc.forkFinally
--                (evalStateT (runContT action (return)) exit)
----                (\e -> handler e)
--                (\e -> void $ runStateT (runContT (handler e) (liftIO . exit)) exit) -- ???
--        return (tid, exit)
--    cont tid
