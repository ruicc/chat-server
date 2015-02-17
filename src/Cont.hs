module Main where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Exception
import System.IO

type Conc a = ContT () IO a

runConcurrent :: (a -> IO ()) -> Conc a -> IO ()
runConcurrent = flip runContT

main :: IO ()
main = runConcurrent putStrLn $ do
    path <- liftIO $ getLine
    hdl <- liftIO $ openFile path ReadMode
    exitStr <- callCC $ \ exit -> do
        str <- dosomething hdl `catchM` \ (e :: SomeException) -> do
                liftIO $ hClose hdl
                liftIO $ putStrLn "Error occured, handle closed normally."
                exit "Exit!!"
        liftIO $ putStrLn str
--        exit "Exit abnormally..."
        errorOccur str

        return $ str ++ "\n" ++ str
    liftIO $ putStrLn exitStr
    return "String for cont"

catchM :: Exception e => Conc a -> (e -> Conc a) -> Conc a
catchM action handler =
    ContT $ \ cont -> runContT action cont
        `catch` \ err -> runContT (handler err) cont

dosomething :: Handle -> Conc String
dosomething hdl = do
    liftIO $ hGetLine hdl

errorOccur :: String -> Conc ()
errorOccur str = do
    liftIO $ throwIO $ ErrorCall "Heyhey, error Occured"
    liftIO $ putStrLn str

--wedge action = do
--    oldCont <- lift get
--    callCC $ \ exit -> do
--        lift $ put exit
--        action
--    lift $ put oldCont
