module Main where

import           Prelude
import           Concurrent
import           Control.Monad.Cont
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import           Data.Monoid
import           System.IO


main = do
    C.forkIO $ runCIO return $ do
        hdl <- liftIO $ openFile "LICENSE" ReadMode
        loop hdl (0 :: Int)
    C.threadDelay $ 30 * 1000 * 1000

loop hdl n = do
    let
        finalizer = liftIO $ putStrLn "fin"
        action = do
            liftIO $ putStrLn $ "heyhey " <> show n
--            throwCIO $ E.ErrorCall "heyhey"
    finally return action finalizer
    loop hdl $ succ n
