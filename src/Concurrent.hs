module Concurrent where


import           Prelude as P
--import           Control.Applicative
import           Control.Monad.Cont
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.STM as S
import qualified Control.Concurrent.Async as As
import qualified Control.Exception as E
import           Control.Exception (Exception(..))
import           System.Mem.Weak
--import           System.IO
--import           Data.Monoid


type ThreadId = Conc.ThreadId

type CIO r a = ContT r IO a

type CSTM r a = ContT r S.STM a

type Concurrent a = CIO () a

runCIO :: (a -> IO r) -> CIO r a -> IO r
runCIO k action = runContT action (\a -> k a)
--{-# INLINE runCIO #-}

runCSTM :: (a -> S.STM r) -> CSTM r a -> S.STM r
runCSTM k action = runContT action (\a -> k a)
--{-# INLINE runCSTM #-}

atomically :: (a -> S.STM r') -> CSTM r' a -> CIO r r'
atomically k action = liftIO $ S.atomically $ runCSTM (\a -> k a) action
--{-# INLINE atomically #-}

atomically_ :: CSTM r' r' -> CIO r r'
atomically_ = atomically return
--{-# INLINE atomically_ #-}

runConcurrent :: Concurrent a -> IO ()
runConcurrent action = runCIO (const $ return ()) action
--{-# INLINE runConcurrent #-}

runSTM :: S.STM a -> CIO r a
runSTM m = liftIO $ S.atomically m
--{-# INLINE runSTM #-}

liftSTM :: S.STM a -> CSTM r a
liftSTM m = ContT (\k -> m >>= k)
--{-# INLINE liftSTM #-}


------------------------------------------------------------------------------------------
-- | Exception

type SomeException = E.SomeException
type Handler = E.Handler

catch :: E.Exception e => (a -> IO r') -> CIO r' a -> (e -> CIO r' a) -> CIO r r'
catch k action handler =
    callCC $ \ exit -> do
        ContT $ \ (k' :: a -> IO r) -> do -- IO
                r' <- runContT action k `E.catch` \ e -> runContT (handler e) (\a -> k a)
                runContT (exit r') k'

catch_ :: E.Exception e => CIO a a -> (e -> CIO a a) -> CIO r a
catch_ = catch (\a -> return a)

handle :: E.Exception e => (a -> IO b) -> (e -> CIO b a) -> CIO b a -> CIO r b
handle k handler action = catch (\a -> k a) action handler

handle_ :: E.Exception e => (e -> CIO a a) -> CIO a a -> CIO r a
handle_ = handle (\a -> return a)

onException :: (a -> IO r') -> CIO r' a -> CIO r' b -> CIO r r'
onException k action handler =
    catch' action $ \ (e :: SomeException) -> do
        _ <- handler
        liftIO $ E.throwIO e
  where
    catch' = catch (\a -> k a)

onException_ :: CIO a a -> CIO a b -> CIO r a
onException_ = onException (\a -> return a)

try :: E.Exception e => (a -> IO r') -> CIO r' a -> CIO r (Either e r')
try k action =
    callCC $ \ exit -> do
        ContT $ \ k' -> do -- IO
                ei <- E.try $ runContT action (\a -> k a)
                runContT (exit ei) k'

try_ :: E.Exception e => CIO a a -> CIO r (Either e a)
try_ = try (\a -> return a)

mask
    :: (a -> IO r') -- ^ Last action to feed to 2nd arg
    -> ((forall s b. CIO s b -> CIO s b) -> CIO r' a)
    -> CIO r r'
mask k userAction =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- IO
            r' <- E.mask $ \ (unblock :: forall a. IO a -> IO a) ->
                let
                    restore :: forall r a. CIO r a -> CIO r a
                    restore act = ContT $ \k'' -> unblock (runContT act k'')
                in
                    runContT (userAction restore) (\a -> k a)
            runContT (exit r') k'

mask_
    :: ((forall s b. CIO s b -> CIO s b) -> CIO a a)
    -> CIO r a
mask_ = mask (\a -> return a)

throwTo :: Exception e => ThreadId -> e -> CIO r ()
throwTo tid e = liftIO $ Conc.throwTo tid e

throwCIO :: Exception e => e -> CIO r a
throwCIO = liftIO . E.throwIO

bracket
    :: (c -> IO r')
    -> CIO r' a -- ^ before (typically, gaining a resource)
    -> (a -> CIO r' b) -- ^ after (release the resrouce)
    -> (a -> CIO r' c) -- ^ action (use the resrouce)
    -> CIO r r'
bracket k before after action =
    mask_ $ \restore -> do -- CIO r'
        a <- before
        r <- onException' (restore (action a)) (after a)
        _ <- after a
        return r
  where
    onException' = onException (\a -> k a)

finally
    :: (a -> IO r') -- ^ last action to feed
    -> CIO r' a -- ^ action
    -> CIO r' t -- ^ finalizer
    -> CIO r r'
finally k action finalizer =
    mask_ $ \restore -> do -- CIO r'
        r' <- onException' (restore action) finalizer
        _ <- finalizer
        return r'
  where
    onException' = onException (\a -> k a)

finally_
    :: CIO a a -- ^ action
    -> CIO a t -- ^ finalizer
    -> CIO r a
finally_ = finally (\a -> return a)

------------------------------------------------------------------------------------------
-- | Concurrent

myThreadId :: CIO r ThreadId
myThreadId = liftIO Conc.myThreadId

fork :: (a -> IO r') -> CIO r' a -> CIO r ThreadId
fork k action = liftIO $ Conc.forkIO (void $ runCIO (\a -> k a) action)

fork_ :: CIO () () -> CIO r ThreadId
fork_ action = liftIO $ Conc.forkIO $ runCIO (\ () -> return ()) action

forkFinally
    :: (a -> IO r')
    -> CIO r' a
    -> (Either SomeException r' -> CIO r'' r'')
    -> CIO r ThreadId
forkFinally k action finalizer =
    mask_ $ \ restore ->
        fork
            (\ ei -> runCIO return (finalizer ei))
            (try (\a -> k a) $ restore action)

forkFinally_
    :: CIO a a
    -> (Either SomeException a -> CIO r' r')
    -> CIO r ThreadId
forkFinally_ = forkFinally return

----forkWithUnmask :: ((forall a. Concurrent a -> Concurrent a) -> Concurrent ()) -> Concurrent ThreadId
----forkWithUnmask userAction = liftIO $ Conc.forkIOWithUnmask $ \ (unmaskIO :: forall a. IO a -> IO a) ->
----    let
----        unmask :: Concurrent a -> Concurrent a
----        unmask action = liftIO $ unmaskIO $ runConcurrent return action
----    in
----        runConcurrent return (userAction unmask)

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
orElse m n = ContT $ \ k -> S.orElse (runCSTM (\a -> k a) m) (runCSTM (\a -> k a) n)

check :: Bool -> CSTM r ()
check = liftSTM . S.check


catchSTM :: Exception e => (a -> STM r') -> CSTM r' a -> (e -> CSTM r' a) -> CSTM r r'
catchSTM k action handler =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- STM
            r' <- (runContT action (\a -> k a)) `S.catchSTM` (\ e -> runContT (handler e) (\a -> k a))
            runContT (exit r') k'

-- | TVar

newTVar :: a -> CSTM r (TVar a)
newTVar v = liftSTM $ S.newTVar v
--{-# INLINE newTVar #-}

newTVarCIO :: a -> CIO r (TVar a)
newTVarCIO v = liftIO $ S.newTVarIO v
--{-# INLINE newTVarCIO #-}

readTVar :: TVar a -> CSTM r a
readTVar v = liftSTM $ S.readTVar v
--{-# INLINE readTVar #-}

readTVarCIO :: TVar a -> CIO r a
readTVarCIO tv = liftIO $ S.readTVarIO tv
--{-# INLINE readTVarCIO #-}

writeTVar :: TVar a -> a -> CSTM r ()
writeTVar tv v = liftSTM $ S.writeTVar tv v
--{-# INLINE writeTVar #-}

modifyTVar :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar tv v  = liftSTM $ S.modifyTVar tv v
--{-# INLINE modifyTVar #-}

modifyTVar' :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar' tv v = liftSTM $ S.modifyTVar' tv v
--{-# INLINE modifyTVar' #-}

swapTVar :: TVar a -> a -> CSTM r a
swapTVar tv v = liftSTM $ S.swapTVar tv v
--{-# INLINE swapTVar #-}

registerDelay :: Int -> CIO r (TVar Bool)
registerDelay v = liftIO $ S.registerDelay v
--{-# INLINE registerDelay #-}

mkWeakTVar :: TVar a -> CIO () () -> CIO r (Weak (TVar a))
mkWeakTVar tv finalizer = liftIO $ S.mkWeakTVar tv (runCIO (\a -> return a) finalizer)
--{-# INLINE mkWeakTVar #-}

-- | TChan

newTChan :: CSTM r (TChan a)
newTChan = liftSTM S.newTChan
--{-# INLINE newTChan #-}

newTChanCIO :: CIO r (TChan a)
newTChanCIO = liftIO S.newTChanIO
--{-# INLINE newTChanCIO #-}

newBroadcastTChan :: CSTM r (TChan a)
newBroadcastTChan = liftSTM S.newBroadcastTChan
--{-# INLINE newBroadcastTChan #-}

dupTChan :: TChan a -> CSTM r (TChan a)
dupTChan tc = liftSTM $ S.dupTChan tc
--{-# INLINE dupTChan #-}

cloneTChan :: TChan a -> CSTM r (TChan a)
cloneTChan tc = liftSTM $ S.cloneTChan tc
--{-# INLINE cloneTChan #-}

readTChan :: TChan a -> CSTM r a
readTChan tc = liftSTM $ S.readTChan tc
--{-# INLINE readTChan #-}

tryReadTChan :: TChan a -> CSTM r (Maybe a)
tryReadTChan tc = liftSTM $ S.tryReadTChan tc
--{-# INLINE tryReadTChan #-}

tryPeekTChan :: TChan a -> CSTM r (Maybe a)
tryPeekTChan tc = liftSTM $ S.tryPeekTChan tc
--{-# INLINE tryPeekTChan #-}

writeTChan :: TChan a -> a -> CSTM r ()
writeTChan tc v = liftSTM $ S.writeTChan tc v
--{-# INLINE writeTChan #-}

unGetTChan :: TChan a -> a -> CSTM r ()
unGetTChan tc v = liftSTM $ S.unGetTChan tc v
--{-# INLINE unGetTChan #-}

isEmptyTChan :: TChan a -> CSTM r Bool
isEmptyTChan tc = liftSTM $ S.isEmptyTChan tc
--{-# INLINE isEmptyTChan #-}


-- | TMVar

newTMVar :: a -> CSTM r (TMVar a)
newTMVar v = liftSTM $ S.newTMVar v
--{-# INLINE newTMVar #-}

newEmptyTMVar :: CSTM r (TMVar a)
newEmptyTMVar = liftSTM S.newEmptyTMVar
--{-# INLINE newEmptyTMVar #-}

newTMVarCIO :: a -> CIO r (TMVar a) 
newTMVarCIO v = liftIO $ S.newTMVarIO v
--{-# INLINE newTMVarCIO #-}

newEmptyTMVarCIO :: CIO r (TMVar a) 
newEmptyTMVarCIO = liftIO S.newEmptyTMVarIO
--{-# INLINE newEmptyTMVarCIO #-}

takeTMVar :: TMVar a -> CSTM r a
takeTMVar tm = liftSTM $ S.takeTMVar tm
--{-# INLINE takeTMVar #-}

putTMVar :: TMVar a -> a -> CSTM r ()
putTMVar tm v = liftSTM $ S.putTMVar tm v
--{-# INLINE putTMVar #-}

readTMVar :: TMVar a -> CSTM r a 
readTMVar tm = liftSTM $ S.readTMVar tm
--{-# INLINE readTMVar #-}

tryReadTMVar :: TMVar a -> CSTM r (Maybe a)
tryReadTMVar tm = liftSTM $ S.tryReadTMVar tm
--{-# INLINE tryReadTMVar #-}

swapTMVar :: TMVar a -> a -> CSTM r a
swapTMVar tm v = liftSTM $ S.swapTMVar tm v
--{-# INLINE swapTMVar #-}

tryTakeTMVar :: TMVar a -> CSTM r (Maybe a)
tryTakeTMVar tm = liftSTM $ S.tryTakeTMVar tm
--{-# INLINE tryTakeTMVar #-}

tryPutTMVar :: TMVar a -> a -> CSTM r Bool
tryPutTMVar tm v = liftSTM $ S.tryPutTMVar tm v
--{-# INLINE tryPutTMVar #-}

isEmptyTMVar :: TMVar a -> CSTM r Bool
isEmptyTMVar tm = liftSTM $ S.isEmptyTMVar tm
--{-# INLINE isEmptyTMVar #-}

--mkWeakTMVar :: TMVar a -> CIO () () -> CIO r (Weak (TMVar a))
--mkWeakTMVar tmv finalizer = liftIO $ S.mkWeakTMVar tmv (runCIO return finalizer)

------------------------------------------------------------------------------------------
-- | TQueue

type TQueue = S.TQueue

newTQueue :: CSTM r (TQueue a)
newTQueue = liftSTM S.newTQueue

newTQueueCIO :: CIO r (TQueue a)
newTQueueCIO = liftIO S.newTQueueIO

readTQueue :: TQueue a -> CSTM r a
readTQueue q = liftSTM $ S.readTQueue q

tryReadTQueue :: TQueue a -> CSTM r (Maybe a)
tryReadTQueue q = liftSTM $ S.tryReadTQueue q

peekTQueue :: TQueue a -> CSTM r a
peekTQueue q = liftSTM $ S.peekTQueue q

tryPeekTQueue :: TQueue a -> CSTM r (Maybe a)
tryPeekTQueue q = liftSTM $ S.tryPeekTQueue q

writeTQueue :: TQueue a -> a -> CSTM r ()
writeTQueue q v = liftSTM $ S.writeTQueue q v

unGetTQueue :: TQueue a -> a -> CSTM r ()
unGetTQueue q v = liftSTM $ S.unGetTQueue q v

isEmptyTQueue :: TQueue a -> CSTM r Bool
isEmptyTQueue q = liftSTM $ S.isEmptyTQueue q


-- | Async

type Async = As.Async

async :: CIO a a -> CIO r (As.Async a)
async m = liftIO $ As.async (runCIO return m)
--{-# INLINE async #-}

wait :: Async a -> CIO r a 
wait = liftIO . As.wait
--{-# INLINE wait #-}

race :: CIO a a -> CIO b b -> CIO r (Either a b)
race c1 c2 = liftIO $ As.race (runCIO return c1) (runCIO return c2)
--{-# INLINE race #-}

race_ :: CIO a a -> CIO b b -> CIO r ()
race_ c1 c2 = liftIO $ As.race_ (runCIO return c1) (runCIO return c2)
--{-# INLINE race_ #-}
