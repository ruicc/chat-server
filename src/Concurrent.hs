module Concurrent where


import           Prelude as P
--import           Control.Applicative
import           Control.Monad.Cont
--import           Control.Monad.State
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import           Control.Exception (Exception(..))
--import           System.IO
--import           Data.Monoid


type ThreadId = Conc.ThreadId
type Concurrent a = forall r. ContT r IO a

runConcurrent :: (a -> IO r) -> Concurrent a -> IO r
runConcurrent cont action = runContT action (liftIO . cont)

runSTM :: STM.STM a -> Concurrent a
runSTM = liftIO . STM.atomically



------------------------------------------------------------------------------------------
-- | Exception

type SomeException = E.SomeException
type Handler = E.Handler

catch :: E.Exception e => Concurrent a -> (e -> Concurrent a) -> Concurrent a
catch action handler =
    callCC $ \exit -> do
        ContT $ \cont -> runContT action cont
                    `E.catch` \e -> runContT (handler e) (\a -> runContT (exit a) return)

handle :: E.Exception e => (e -> Concurrent a) -> Concurrent a -> Concurrent a
handle = flip catch

onException :: Concurrent a -> Concurrent b -> Concurrent a
onException action handler =
    action `catch` \ (e :: SomeException) -> do
        _ <- handler
        liftIO $ E.throwIO e

try :: E.Exception e => Concurrent a -> Concurrent (Either e a)
try action = liftIO $ E.try (runConcurrent return action)

mask :: ((forall a. Concurrent a -> Concurrent a) -> Concurrent b) -> Concurrent b
mask userAction = liftIO $ E.mask $ \ (unblock :: forall a. IO a -> IO a) ->
    let
        restore :: Concurrent a -> Concurrent a
        restore act = liftIO $ unblock $ runConcurrent return act
    in
        runConcurrent return (userAction restore)

throwTo :: Exception e => ThreadId -> e -> Concurrent ()
throwTo tid e = liftIO $ Conc.throwTo tid e

throwC :: Exception e => e -> Concurrent a
throwC = liftIO . E.throwIO

bracket :: Concurrent a -> (a -> Concurrent b) -> (a -> Concurrent c) -> Concurrent c
bracket before after thing = mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

finally :: Concurrent a -> Concurrent b -> Concurrent a
finally action final = mask $ \restore -> do
    r <- restore action `onException` final 
    _ <- final
    return r

------------------------------------------------------------------------------------------
-- | Concurrent

myThreadId :: Concurrent ThreadId
myThreadId = liftIO Conc.myThreadId

forkC :: Concurrent () -> Concurrent ThreadId
forkC action = liftIO $ Conc.forkIO $ runConcurrent return action

forkFinally :: Concurrent a -> (Either SomeException a -> Concurrent ()) -> Concurrent ThreadId
forkFinally action and_then = do
    mask $ \ restore ->
        forkC $ try (restore action) >>= and_then

forkCWithUnmask :: ((forall a. Concurrent a -> Concurrent a) -> Concurrent ()) -> Concurrent ThreadId
forkCWithUnmask userAction = liftIO $ Conc.forkIOWithUnmask $ \ (unmaskIO :: forall a. IO a -> IO a) -> 
    let
        unmask :: Concurrent a -> Concurrent a
        unmask action = liftIO $ unmaskIO $ runConcurrent return action
    in
        runConcurrent return (userAction unmask)

killThread :: ThreadId -> Concurrent ()
killThread = liftIO . Conc.killThread

threadDelay :: Int -> Concurrent ()
threadDelay = liftIO . Conc.threadDelay
