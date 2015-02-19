module Concurrent2 where


import           Prelude as P
--import           Control.Applicative
import           Control.Monad.Cont
--import           Control.Monad.State
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.STM as S
import qualified Control.Exception as E
import           Control.Exception (Exception(..))
import           System.Mem.Weak
--import           System.IO
--import           Data.Monoid


type ThreadId = Conc.ThreadId
type CIO r a = ContT r IO a
type CSTM r a = ContT r S.STM a

--type Concurrent a = CIO () a
--type ConcurrentSTM a = ContT () S.STM a

runCIO :: (a -> IO r) -> CIO r a -> IO r
runCIO cont action = runContT action cont

runCSTM :: (a -> S.STM r) -> CSTM r a -> S.STM r
runCSTM cont action = runContT action cont

atomically :: (a -> S.STM r') -> CSTM r' a -> CIO r r'
atomically cont action = liftIO $ S.atomically $ runCSTM cont action

atomically_ :: CSTM r' r' -> CIO r r'
atomically_ = atomically return

--runConcurrent :: Concurrent a -> IO ()
--runConcurrent action = runCIO (const $ return ()) action

runSTM :: S.STM a -> CIO r a
runSTM = liftIO . S.atomically

liftSTM :: S.STM a -> CSTM r a
liftSTM m = ContT (m >>=)


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

try :: E.Exception e => CIO r a -> CIO r (Either e a)
try action = fmap Right action `catch` \ e -> return $ Left e


mask :: ((forall r' a. CIO r' a -> CIO r' a) -> CIO r b) -> CIO r b
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
finally action finalizer = mask $ \restore -> do
    r <- restore action `onException` finalizer 
    _ <- finalizer
    return r

------------------------------------------------------------------------------------------
-- | Concurrent

myThreadId :: CIO r ThreadId
myThreadId = liftIO Conc.myThreadId

forkC :: (a -> IO r') -> CIO r' a -> CIO r ThreadId
forkC k action = liftIO $ Conc.forkIO (void $ runCIO k action)

forkC_ :: CIO () () -> CIO r ThreadId
forkC_ action = liftIO $ Conc.forkIO $ runCIO (\ () -> return ()) action

forkFinally :: CIO r' a -> (Either SomeException a -> CIO r' r') -> CIO r ThreadId
forkFinally action finalizer =
    mask $ \ restore ->
        forkC
            (\ e -> runCIO return (finalizer e))
            (try $ restore action)

--forkCWithUnmask :: ((forall a. Concurrent a -> Concurrent a) -> Concurrent ()) -> Concurrent ThreadId
--forkCWithUnmask userAction = liftIO $ Conc.forkIOWithUnmask $ \ (unmaskIO :: forall a. IO a -> IO a) -> 
--    let
--        unmask :: Concurrent a -> Concurrent a
--        unmask action = liftIO $ unmaskIO $ runConcurrent return action
--    in
--        runConcurrent return (userAction unmask)

killThread :: ThreadId -> CIO r ()
killThread = liftIO . Conc.killThread

threadDelay :: Int -> CIO r ()
threadDelay = liftIO . Conc.threadDelay



------------------------------------------------------------------------------------------
-- | STM

type STM = S.STM
type TVar = S.TVar
type TChan = S.TChan

retry :: CSTM r a
retry = liftSTM S.retry

orElse :: CSTM r a -> CSTM r a -> CSTM r a
orElse m n = ContT $ \ k -> S.orElse (runCSTM k m) (runCSTM k n)

check :: Bool -> CSTM r ()
check = liftSTM . S.check

catchSTM :: Exception e => CSTM r a -> (e -> CSTM r a) -> CSTM r a
catchSTM action handler =
    callCC $ \ exit ->
        ContT $ \ k -> S.catchSTM (runCSTM k action) (\e -> runCSTM (\a -> runCSTM return (exit a)) $ handler e)

newTVar :: a -> CSTM r (TVar a)
newTVar = liftSTM . S.newTVar

newTVarCIO :: a -> CIO r (TVar a)
newTVarCIO = liftIO . S.newTVarIO

readTVar :: TVar a -> CSTM r a
readTVar = liftSTM . S.readTVar

readTVarCIO :: TVar a -> CIO r a
readTVarCIO = liftIO . S.readTVarIO

writeTVar :: TVar a -> a -> CSTM r ()
writeTVar = (liftSTM .) . S.writeTVar

modifyTVar :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar = (liftSTM .) . S.modifyTVar

modifyTVar' :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar' = (liftSTM .) . S.modifyTVar'

swapTVar :: TVar a -> a -> CSTM r a
swapTVar = (liftSTM .) . S.swapTVar

registerDelay :: Int -> CIO r (TVar Bool)
registerDelay = liftIO . S.registerDelay

mkWeakTVar :: TVar a -> CIO () () -> CIO r (Weak (TVar a))
mkWeakTVar tv finalizer = liftIO $ S.mkWeakTVar tv (runCIO (return) finalizer)

newTChan :: CSTM r (TChan a)
newTChan = liftSTM S.newTChan

newTChanCIO :: CIO r (TChan a)
newTChanCIO = liftIO S.newTChanIO

newBroadcastTChan :: CSTM r (TChan a)
newBroadcastTChan = liftSTM S.newBroadcastTChan

dupTChan :: TChan a -> CSTM r (TChan a)
dupTChan = liftSTM . S.dupTChan

cloneTChan :: TChan a -> CSTM r (TChan a)
cloneTChan = liftSTM . S.cloneTChan

readTChan :: TChan a -> CSTM r a
readTChan = liftSTM . S.readTChan

tryReadTChan :: TChan a -> CSTM r (Maybe a)
tryReadTChan = liftSTM . S.tryReadTChan

tryPeekTChan :: TChan a -> CSTM r (Maybe a)
tryPeekTChan = liftSTM . S.tryPeekTChan

writeTChan :: TChan a -> a -> CSTM r ()
writeTChan = (liftSTM .) . S.writeTChan

unGetTChan :: TChan a -> a -> CSTM r ()
unGetTChan = (liftSTM .) . S.unGetTChan

isEmptyTChan :: TChan a -> CSTM r Bool
isEmptyTChan = liftSTM . S.isEmptyTChan
