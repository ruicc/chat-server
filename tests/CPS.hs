module Main where


--import Control.Applicative
import Control.Monad.Cont
import qualified Control.Exception as E
import System.IO
import Data.Monoid
import Concurrent (CIO, runCIO, forkFinally, throwCIO, threadDelay)

main = void $ runCIO return $ do
--    path <- liftIO $ getLine
    let path = "LICENSE"

    hdl <- liftIO $ openFile path ReadMode

    liftIO $ putStrLn "a lot let"
    main_alotlet hdl
    liftIO $ putStrLn "few let"
    main_fewlet hdl
    threadDelay $ 500 * 1000


main_fewlet :: Handle -> CIO r ()
main_fewlet hdl = void $ forkFinally (example1 hdl) (\_ -> liftIO $ hClose hdl)

main_alotlet :: Handle -> CIO r ()
main_alotlet hdl = do
    let
        example_alotlet :: CIO r ()
        example_alotlet = do
            str <- dosomething hdl
            liftIO $ putStrLn str
--            _ <- errorOccur str
            return ()
        hoge :: CIO r ()
        hoge = do
            liftIO $ putStrLn "hoge"
            return ()
        fuga :: CIO r ()
        fuga = do
            _ <- errorOccur "fuga"
            return ()

    void $ forkFinally (example_alotlet >> hoge >> fuga) (\_ -> liftIO $ hClose hdl)


example1 :: Handle -> CIO r ()
example1 hdl = do
    str <- dosomething hdl
    liftIO $ putStrLn str
    _ <- errorOccur str
    return ()
{-# INLINE example1 #-}

dosomething :: Handle -> CIO r String
dosomething hdl = liftIO $ hGetLine hdl

errorOccur :: String -> CIO r String
errorOccur str = do
    liftIO $ putStrLn str
    throwCIO $ E.ErrorCall "Heyhey, error occured"
