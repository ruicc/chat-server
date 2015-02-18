module Concurrent where


import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import System.IO
import Data.Monoid


type Concurrent a = forall r. ContT r IO a

runConcurrent :: (a -> IO r) -> Concurrent a -> IO r
runConcurrent cont action = runContT action (liftIO . cont)

runSTM :: STM.STM a -> Concurrent a
runSTM = liftIO . STM.atomically





catch :: E.Exception e => Concurrent a -> (e -> Concurrent a) -> Concurrent a
catch action handler =
    callCC $ \exit -> do
        ContT $ \cont -> runContT action cont
                    `E.catch` \e -> runContT (handler e) (\a -> runContT (exit a) return)

handle :: E.Exception e => (e -> Concurrent a) -> Concurrent a -> Concurrent a
handle = flip catch

onException :: Concurrent a -> Concurrent b -> Concurrent a
onException action handler =
    action `catch` \ (e :: E.SomeException) -> do
        _ <- handler
        liftIO $ E.throwIO e

forkFinally :: Concurrent a -> (Either E.SomeException a -> Concurrent ()) -> Concurrent Conc.ThreadId
forkFinally action and_then = do
    mask $ \ restore ->
        fork $ try (restore action) >>= and_then

fork :: Concurrent () -> Concurrent Conc.ThreadId
fork action = liftIO $ Conc.forkIO $ runConcurrent return action

try :: E.Exception e => Concurrent a -> Concurrent (Either e a)
try action = liftIO $ E.try (runConcurrent return action)

wait :: Int -> Concurrent ()
wait = liftIO . Conc.threadDelay

mask :: ((forall a. Concurrent a -> Concurrent a) -> Concurrent b) -> Concurrent b
mask userAction = liftIO $ E.mask $ \ (unblock :: forall a. IO a -> IO a) ->
    let
        restore :: Concurrent a -> Concurrent a
        restore act = liftIO $ unblock $ runConcurrent return act
    in
        runConcurrent return (userAction restore)
