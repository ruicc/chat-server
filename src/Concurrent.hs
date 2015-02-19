module Concurrent where


--import           Prelude as P
--import           Control.Applicative
import           Control.Monad.Cont
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
runCIO k action = runContT action k

runCSTM :: (a -> S.STM r) -> CSTM r a -> S.STM r
runCSTM k action = runContT action k

atomically :: (a -> S.STM r') -> CSTM r' a -> CIO r r'
atomically k action = liftIO $ S.atomically $ runCSTM k action

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
    callCC $ \ exit -> do
        ContT $ \ k -> runContT action k
                    `E.catch` \ e -> runContT (handler e) (\ a -> runContT (exit a) return)

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
mask userAction = ContT $ \ k -> E.mask $ \ (unblock :: forall a. IO a -> IO a) ->
    let
        restore :: forall r a. CIO r a -> CIO r a
        restore act = ContT $ \cont' -> unblock $ runCIO cont' act
    in
        runCIO k (userAction restore)

throwTo :: Exception e => ThreadId -> e -> CIO r ()
throwTo tid e = liftIO $ Conc.throwTo tid e

throwCIO :: Exception e => e -> CIO r a
throwCIO = liftIO . E.throwIO

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

fork :: (a -> IO r') -> CIO r' a -> CIO r ThreadId
fork k action = liftIO $ Conc.forkIO (void $ runCIO k action)

fork_ :: CIO () () -> CIO r ThreadId
fork_ action = liftIO $ Conc.forkIO $ runCIO (\ () -> return ()) action

forkFinally :: CIO r' a -> (Either SomeException a -> CIO r' r') -> CIO r ThreadId
forkFinally action finalizer =
    mask $ \ restore ->
        fork
            (\ e -> runCIO return (finalizer e))
            (try $ restore action)

--forkWithUnmask :: ((forall a. Concurrent a -> Concurrent a) -> Concurrent ()) -> Concurrent ThreadId
--forkWithUnmask userAction = liftIO $ Conc.forkIOWithUnmask $ \ (unmaskIO :: forall a. IO a -> IO a) -> 
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
type TMVar = S.TMVar

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

-- | TVar

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
mkWeakTVar tv finalizer = liftIO $ S.mkWeakTVar tv (runCIO return finalizer)

-- | TChan

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

-- | TMVar

newTMVar :: a -> CSTM r (TMVar a)
newTMVar = liftSTM . S.newTMVar

newEmptyTMVar :: CSTM r (TMVar a)
newEmptyTMVar = liftSTM S.newEmptyTMVar

newTMVarCIO :: a -> CIO r (TMVar a) 
newTMVarCIO = liftIO . S.newTMVarIO

newEmptyTMVarCIO :: CIO r (TMVar a) 
newEmptyTMVarCIO = liftIO S.newEmptyTMVarIO

takeTMVar :: TMVar a -> CSTM r a
takeTMVar = liftSTM . S.takeTMVar

putTMVar :: TMVar a -> a -> CSTM r ()
putTMVar = (liftSTM .) . S.putTMVar

readTMVar :: TMVar a -> CSTM r a 
readTMVar = liftSTM . S.readTMVar

tryReadTMVar :: TMVar a -> CSTM r (Maybe a)
tryReadTMVar = liftSTM . S.tryReadTMVar

swapTMVar :: TMVar a -> a -> CSTM r a
swapTMVar = (liftSTM .) . S.swapTMVar

tryTakeTMVar :: TMVar a -> CSTM r (Maybe a)
tryTakeTMVar = liftSTM . S.tryTakeTMVar

tryPutTMVar :: TMVar a -> a -> CSTM r Bool
tryPutTMVar = (liftSTM .) . S.tryPutTMVar

isEmptyTMVar :: TMVar a -> CSTM r Bool
isEmptyTMVar = liftSTM . S.isEmptyTMVar

--mkWeakTMVar :: TMVar a -> CIO () () -> CIO r (Weak (TMVar a))
--mkWeakTMVar tmv finalizer = liftIO $ S.mkWeakTMVar tmv (runCIO return finalizer)
