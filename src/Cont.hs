module Main where

--import Control.Applicative
import Control.Monad.Cont
--import Control.Monad.State
--import qualified Control.Concurrent as Conc
--import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import System.IO
import Data.Monoid
import Concurrent2

main :: IO ()
main = void $ runCIO return $ do
--    path <- liftIO $ getLine
    let path = "core.dump"

    hdl <- liftIO $ openFile path ReadMode

    let
        errorhandler :: Either SomeException () -> CIO String String
        errorhandler (Left e) = do
            liftIO $ hClose hdl
            liftIO $ putStrLn $ "Err: " <> show e
            liftIO $ putStrLn "Log: Handle closed despite an error."
            return "NG"
        errorhandler (Right a) = do
            liftIO $ hClose hdl
            liftIO $ putStrLn $ "Log: " <> show a
            liftIO $ putStrLn "Log: Handle closed normally."
            return "OK"

        example1 :: CIO String ()
        example1 = do
            str <- dosomething hdl
            str1 <- dosomething hdl
            str2 <- dosomething hdl `onException` (liftIO $ putStrLn "Err: onException 2")

            liftIO $ putStrLn str
            liftIO $ putStrLn str1
            liftIO $ putStrLn str2
            _ <- errorOccur str `onException` (liftIO $ putStrLn "Err: onException 1")
            return ()

    tid <- forkFinally example1 errorhandler

--    liftIO $ putStrLn $ "Exit string: " <> exitStr


    tv :: TVar Int <- newTVarCIO 0

    forkC_ $ do
        let
            loop = do
                atomically_ $ modifyTVar' tv succ
                threadDelay $ 100 * 1000
                loop
        loop

    threadDelay $ 1 * 1000 * 1000

    n <- atomically_ $ readTVar tv

    -- Nested Actions: error? => OK!!
    join $ atomically_ $ readPrint tv

    liftIO $ putStrLn $ "fork & STM : " <> show n



dosomething :: Handle -> CIO r String
dosomething hdl = do
    liftIO $ hGetLine hdl

errorOccur :: String -> CIO r String
errorOccur str = do
    _ <- throwC $ E.ErrorCall "Heyhey, error Occured"
    liftIO $ putStrLn str
    return str

readPrint :: TVar Int -> CSTM r' (CIO r ())
readPrint tv = do
    n <- readTVar tv
    return $ do
        liftIO $ putStrLn $ "Nested Actions : " <> show n
