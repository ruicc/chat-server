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
runCIO k action = runContT action k
{-# INLINE runCIO #-}

runCSTM :: (a -> S.STM r) -> CSTM r a -> S.STM r
runCSTM k action = runContT action k
{-# INLINE runCSTM #-}

atomically :: (a -> S.STM r') -> CSTM r' a -> CIO r r'
atomically k action = liftIO $ S.atomically $ runCSTM k action
{-# INLINE atomically #-}

atomically_ :: CSTM r' r' -> CIO r r'
atomically_ = atomically return
{-# INLINE atomically_ #-}

runConcurrent :: Concurrent a -> IO ()
runConcurrent action = runCIO (const $ return ()) action
{-# INLINE runConcurrent #-}

runSTM :: S.STM a -> CIO r a
runSTM = liftIO . S.atomically
{-# INLINE runSTM #-}

liftSTM :: S.STM a -> CSTM r a
liftSTM m = ContT (m >>=)
{-# INLINE liftSTM #-}


------------------------------------------------------------------------------------------
-- | Exception

type SomeException = E.SomeException
type Handler = E.Handler

catch :: E.Exception e => (a -> IO r') -> CIO r' a -> (e -> CIO r' a) -> CIO r r'
catch k action handler =
    callCC $ \ exit -> do
        ContT $ \ (k' :: a -> IO r) -> do -- IO
                r' <- runContT action k `E.catch` \ e -> runContT (handler e) k
                runContT (exit r') k'

catch_ :: E.Exception e => CIO a a -> (e -> CIO a a) -> CIO r a
catch_ = catch return

handle :: E.Exception e => (a -> IO b) -> (e -> CIO b a) -> CIO b a -> CIO r b
handle k handler action = catch k action handler

handle_ :: E.Exception e => (e -> CIO a a) -> CIO a a -> CIO r a
handle_ = handle return

onException :: (a -> IO r') -> CIO r' a -> CIO r' b -> CIO r r'
onException k action handler =
    catch' action $ \ (e :: SomeException) -> do
        _ <- handler
        liftIO $ E.throwIO e
  where
    catch' = catch k

onException_ :: CIO a a -> CIO a b -> CIO r a
onException_ = onException return

try :: E.Exception e => (a -> IO r') -> CIO r' a -> CIO r (Either e r')
try k action =
    callCC $ \ exit -> do
        ContT $ \ k' -> do -- IO
                ei <- E.try $ runContT action k
                runContT (exit ei) k'

try_ :: E.Exception e => CIO a a -> CIO r (Either e a)
try_ = try return

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
                    runContT (userAction restore) k
            runContT (exit r') k'

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
    mask return $ \restore -> do -- CIO r'
        a <- before
        r <- onException' (restore (action a)) (after a)
        _ <- after a
        return r
  where
    onException' = onException k

finally
    :: (a -> IO r') -- ^ last action to feed
    -> CIO r' a -- ^ action
    -> CIO r' t -- ^ finalizer
    -> CIO r r'
finally k action finalizer =
    mask return $ \restore -> do -- CIO r'
        r' <- onException' (restore action) finalizer
        _ <- finalizer
        return r'
  where
    onException' = onException k


------------------------------------------------------------------------------------------
-- | Concurrent

myThreadId :: CIO r ThreadId
myThreadId = liftIO Conc.myThreadId

fork :: (a -> IO r') -> CIO r' a -> CIO r ThreadId
fork k action = liftIO $ Conc.forkIO (void $ runCIO k action)

fork_ :: CIO () () -> CIO r ThreadId
fork_ action = liftIO $ Conc.forkIO $ runCIO (\ () -> return ()) action

forkFinally
    :: (a -> IO r')
    -> CIO r' a
    -> (Either SomeException r' -> CIO r'' r'')
    -> CIO r ThreadId
forkFinally k action finalizer =
    mask return $ \ restore ->
        fork
            (\ ei -> runCIO return (finalizer ei))
            (try k $ restore action)

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
orElse m n = ContT $ \ k -> S.orElse (runCSTM k m) (runCSTM k n)

check :: Bool -> CSTM r ()
check = liftSTM . S.check


catchSTM :: Exception e => (a -> STM r') -> CSTM r' a -> (e -> CSTM r' a) -> CSTM r r'
catchSTM k action handler =
    callCC $ \ exit ->
        ContT $ \ k' -> do -- STM
            r' <- (runContT action k) `S.catchSTM` (\ e -> runContT (handler e) k)
            runContT (exit r') k'

-- | TVar

newTVar :: a -> CSTM r (TVar a)
newTVar = liftSTM . S.newTVar
{-# INLINE newTVar #-}

newTVarCIO :: a -> CIO r (TVar a)
newTVarCIO = liftIO . S.newTVarIO
{-# INLINE newTVarCIO #-}

readTVar :: TVar a -> CSTM r a
readTVar = liftSTM . S.readTVar
{-# INLINE readTVar #-}

readTVarCIO :: TVar a -> CIO r a
readTVarCIO = liftIO . S.readTVarIO
{-# INLINE readTVarCIO #-}

writeTVar :: TVar a -> a -> CSTM r ()
writeTVar = (liftSTM .) . S.writeTVar
{-# INLINE writeTVar #-}

modifyTVar :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar = (liftSTM .) . S.modifyTVar
{-# INLINE modifyTVar #-}

modifyTVar' :: TVar a -> (a -> a) -> CSTM r ()
modifyTVar' = (liftSTM .) . S.modifyTVar'
{-# INLINE modifyTVar' #-}

swapTVar :: TVar a -> a -> CSTM r a
swapTVar = (liftSTM .) . S.swapTVar
{-# INLINE swapTVar #-}

registerDelay :: Int -> CIO r (TVar Bool)
registerDelay = liftIO . S.registerDelay
{-# INLINE registerDelay #-}

mkWeakTVar :: TVar a -> CIO () () -> CIO r (Weak (TVar a))
mkWeakTVar tv finalizer = liftIO $ S.mkWeakTVar tv (runCIO return finalizer)
{-# INLINE mkWeakTVar #-}

-- | TChan

newTChan :: CSTM r (TChan a)
newTChan = liftSTM S.newTChan
{-# INLINE newTChan #-}

newTChanCIO :: CIO r (TChan a)
newTChanCIO = liftIO S.newTChanIO
{-# INLINE newTChanCIO #-}

newBroadcastTChan :: CSTM r (TChan a)
newBroadcastTChan = liftSTM S.newBroadcastTChan
{-# INLINE newBroadcastTChan #-}

dupTChan :: TChan a -> CSTM r (TChan a)
dupTChan = liftSTM . S.dupTChan
{-# INLINE dupTChan #-}

cloneTChan :: TChan a -> CSTM r (TChan a)
cloneTChan = liftSTM . S.cloneTChan
{-# INLINE cloneTChan #-}

readTChan :: TChan a -> CSTM r a
readTChan = liftSTM . S.readTChan
{-# INLINE readTChan #-}

tryReadTChan :: TChan a -> CSTM r (Maybe a)
tryReadTChan = liftSTM . S.tryReadTChan
{-# INLINE tryReadTChan #-}

tryPeekTChan :: TChan a -> CSTM r (Maybe a)
tryPeekTChan = liftSTM . S.tryPeekTChan
{-# INLINE tryPeekTChan #-}

writeTChan :: TChan a -> a -> CSTM r ()
writeTChan = (liftSTM .) . S.writeTChan
{-# INLINE writeTChan #-}

unGetTChan :: TChan a -> a -> CSTM r ()
unGetTChan = (liftSTM .) . S.unGetTChan
{-# INLINE unGetTChan #-}

isEmptyTChan :: TChan a -> CSTM r Bool
isEmptyTChan = liftSTM . S.isEmptyTChan
{-# INLINE isEmptyTChan #-}


-- | TMVar

newTMVar :: a -> CSTM r (TMVar a)
newTMVar = liftSTM . S.newTMVar
{-# INLINE newTMVar #-}

newEmptyTMVar :: CSTM r (TMVar a)
newEmptyTMVar = liftSTM S.newEmptyTMVar
{-# INLINE newEmptyTMVar #-}

newTMVarCIO :: a -> CIO r (TMVar a) 
newTMVarCIO = liftIO . S.newTMVarIO
{-# INLINE newTMVarCIO #-}

newEmptyTMVarCIO :: CIO r (TMVar a) 
newEmptyTMVarCIO = liftIO S.newEmptyTMVarIO
{-# INLINE newEmptyTMVarCIO #-}

takeTMVar :: TMVar a -> CSTM r a
takeTMVar = liftSTM . S.takeTMVar
{-# INLINE takeTMVar #-}

putTMVar :: TMVar a -> a -> CSTM r ()
putTMVar = (liftSTM .) . S.putTMVar
{-# INLINE putTMVar #-}

readTMVar :: TMVar a -> CSTM r a 
readTMVar = liftSTM . S.readTMVar
{-# INLINE readTMVar #-}

tryReadTMVar :: TMVar a -> CSTM r (Maybe a)
tryReadTMVar = liftSTM . S.tryReadTMVar
{-# INLINE tryReadTMVar #-}

swapTMVar :: TMVar a -> a -> CSTM r a
swapTMVar = (liftSTM .) . S.swapTMVar
{-# INLINE swapTMVar #-}

tryTakeTMVar :: TMVar a -> CSTM r (Maybe a)
tryTakeTMVar = liftSTM . S.tryTakeTMVar
{-# INLINE tryTakeTMVar #-}

tryPutTMVar :: TMVar a -> a -> CSTM r Bool
tryPutTMVar = (liftSTM .) . S.tryPutTMVar
{-# INLINE tryPutTMVar #-}

isEmptyTMVar :: TMVar a -> CSTM r Bool
isEmptyTMVar = liftSTM . S.isEmptyTMVar
{-# INLINE isEmptyTMVar #-}

--mkWeakTMVar :: TMVar a -> CIO () () -> CIO r (Weak (TMVar a))
--mkWeakTMVar tmv finalizer = liftIO $ S.mkWeakTMVar tmv (runCIO return finalizer)


-- | Async

type Async = As.Async

async :: CIO a a -> CIO r (As.Async a)
async m = liftIO $ As.async (runCIO return m)
{-# INLINE async #-}

wait :: Async a -> CIO r a 
wait = liftIO . As.wait
{-# INLINE wait #-}

race :: CIO a a -> CIO b b -> CIO r (Either a b)
race c1 c2 = liftIO $ As.race (runCIO return c1) (runCIO return c2)
{-# INLINE race #-}

race_ :: CIO a a -> CIO b b -> CIO r ()
race_ c1 c2 = liftIO $ As.race_ (runCIO return c1) (runCIO return c2)
{-# INLINE race_ #-}
