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

import           Types

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
        gid = 42
        name = "Alice's"
        capacity = 2
        timestamp = 200
        timeout = 10

        put :: String -> IO ()
        put = putStrLn

        p :: Show a => IO (IO a) -> IO ()
        p act = join act >>= print

        grVal = GroupValue
            gid
            name
            capacity
            timestamp
            timeout

    hSetBuffering stdout LineBuffering

    -- Dummy handle
    hdl <- openFile "LICENSE" ReadMode

    put "-------- Heyhey --------"

    gr :: Group <- newGroup grVal

    cl1 <- newClient hdl
    cl2 <- newClient hdl
    cl3 <- newClient hdl
    cl4 <- newClient hdl

    gr ! AddMember 1 cl1
    gr ! AddMember 2 cl2
    gr ! AddMember 3 cl3
    gr ! AddMember 4 cl4

    gr ! RemoveMember 2
    gr ! RemoveMember 3

    p $ gr !? GetMember 1
    p $ gr !? GetMember 2
    p $ gr !? GetAllMembers

    p $ gr !? GetSessionData
    p $ gr !? PutSessionData (GameSessionData Night 200)

    p $ gr !? ToPreparing
    p $ gr !? ToPlaying
    p $ gr !? ToResult

    kill gr

    put "-------- Finish --------"
