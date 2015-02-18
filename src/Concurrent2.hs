module Concurrent2 where


import           Prelude as P
--import           Control.Applicative
import           Control.Monad.Cont
--import           Control.Monad.State
import qualified Control.Concurrent as Conc
import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import           Control.Exception (Exception(..))
--import           System.IO
--import           Data.Monoid


type ThreadId = Conc.ThreadId
type CIO r a = ContT r IO a
type CSTM r a = ContT r STM a

type Concurrent a = CIO () a
type ConcurrentSTM a = ContT () STM a

runCIO :: (a -> IO r) -> CIO r a -> IO r
runCIO cont action = runContT action cont

runCSTM :: (a -> STM r) -> CSTM r a -> STM r
runCSTM cont action = runContT action cont

runConcurrent :: Concurrent a -> IO ()
runConcurrent action = runCIO (const $ return ()) action


runSTM :: STM a -> Concurrent a
runSTM = liftIO . STM.atomically

--joinC :: Concurrent (Concurrent a) -> Concurrent a
--joinC mm = do
--    let
--        run :: Concurrent a -> IO a
--        run m = runConcurrent return m
--
--    liftIO $ join $ run (fmap run mm)



------------------------------------------------------------------------------------------
-- | Exception

type SomeException = E.SomeException
type Handler = E.Handler

catch :: E.Exception e => CIO r a -> (e -> CIO r a) -> CIO r a
catch action handler =
    callCC $ \exit -> do
        ContT $ \cont -> runContT action cont
                    `E.catch` \e -> runContT (handler e) (\a -> runContT (exit a) return)

handle :: E.Exception e => (e -> CIO r a) -> CIO r a -> CIO r a
handle = flip catch

onException :: CIO r a -> CIO r b -> CIO r a
onException action handler =
    action `catch` \ (e :: SomeException) -> do
        _ <- handler
        liftIO $ E.throwIO e

--try :: E.Exception e => CIO r a -> CIO r (Either e a)
--try action = liftIO $ E.try (runConcurrent return action)

mask :: ((forall a. CIO r a -> CIO r a) -> CIO r b) -> CIO r b
mask userAction = ContT $ \cont -> E.mask $ \ (unblock :: forall a. IO a -> IO a) ->
    let
        restore :: forall r a. CIO r a -> CIO r a
        restore act = ContT $ \cont' -> unblock $ runCIO cont' act
    in
        runCIO cont (userAction restore)

throwTo :: Exception e => ThreadId -> e -> CIO r ()
throwTo tid e = liftIO $ Conc.throwTo tid e

throwC :: Exception e => e -> CIO r a
throwC = liftIO . E.throwIO

bracket :: CIO r a -> (a -> CIO r b) -> (a -> CIO r c) -> CIO r c
bracket before after thing = mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

finally :: CIO r a -> CIO r b -> CIO r a
finally action final = mask $ \restore -> do
    r <- restore action `onException` final 
    _ <- final
    return r

--------------------------------------------------------------------------------------------
---- | Concurrent
--
--myThreadId :: Concurrent ThreadId
--myThreadId = liftIO Conc.myThreadId
--
--forkC :: Concurrent () -> Concurrent ThreadId
--forkC action = liftIO $ Conc.forkIO $ runConcurrent (\() -> return ()) action
--
--forkFinally :: Concurrent a -> (Either SomeException a -> Concurrent ()) -> Concurrent ThreadId
--forkFinally action and_then = do
--    mask $ \ restore ->
--        forkC $ try (restore action) >>= and_then
--
--forkCWithUnmask :: ((forall a. Concurrent a -> Concurrent a) -> Concurrent ()) -> Concurrent ThreadId
--forkCWithUnmask userAction = liftIO $ Conc.forkIOWithUnmask $ \ (unmaskIO :: forall a. IO a -> IO a) -> 
--    let
--        unmask :: Concurrent a -> Concurrent a
--        unmask action = liftIO $ unmaskIO $ runConcurrent return action
--    in
--        runConcurrent return (userAction unmask)
--
--killThread :: ThreadId -> Concurrent ()
--killThread = liftIO . Conc.killThread
--
--threadDelay :: Int -> Concurrent ()
--threadDelay = liftIO . Conc.threadDelay
