module Main where

import           Prelude hiding (lookup)
import           Control.Applicative ((<$>))
import           Control.Monad (join)
import           Control.Concurrent.Object
import           Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.Map as Map

import           Control.Monad.Trans.Cont (ContT)
import           Control.Concurrent.Structured (liftIO)
import qualified Control.Concurrent.Structured as CS

import           System.IO

import           GroupTy

--------------------------------------------------------------------------------
{-
 -  TODO:
 -      * createGroup (Server)
 -      * deleteGroup (Server)
 -      * getGrouip (Server)
 -      * getAll (Server)
 -
 -      * new
 -      * killCanceler (State Timeout)
 -      * changeStatus
 -      * getClient
 -      * handle each status
 -      * addClient/removeClient
 -}


main :: IO ()
main = do
    let
        gid :: Int
        gid = 42

        put :: String -> CS.Concurrent ()
        put = liftIO . putStrLn

        p :: Show a => IO (IO a) -> IO ()
        p act = join act >>= print

    hSetBuffering stdout LineBuffering

    gr :: Group <- newGroup gid

    gr ! AddMember 1 (newClient "Alice" Red)
    gr ! AddMember 2 (newClient "Alice" Blue)
    gr ! AddMember 3 (newClient "Charlie" Blue)
    gr ! AddMember 4 (newClient "Edward" Blue)

    gr ! RemoveMember 2
    gr ! RemoveMember 3

    p $ gr !? GetGroupId
    p $ gr !? GetMember 1
    p $ gr !? GetMember 2
    p $ gr !? GetAllMembers

    p $ gr !? GetSessionData
    p $ gr !? PutSessionData (GameSessionData Night)

    p $ gr !? ToPreparing
    p $ gr !? ToPlaying
    p $ gr !? ToResult

    kill gr
