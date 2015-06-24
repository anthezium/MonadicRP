--{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, Safe, TupleSections #-}
-- TODO: figure out how to trust atomic-primops.  Do I need to build a version of it marked as Trustworthy?
{-# LANGUAGE BangPatterns, DisambiguateRecordFields, FlexibleInstances
           , MagicHash, MultiParamTypeClasses, NamedFieldPuns, RankNTypes 
           , TupleSections, TypeSynonymInstances #-}
module RP 
  ( SRef(), RP(), RPE(), RPR(), RPW(), RPRead
  , ThreadState(..) 
  , newSRef, readSRef, writeSRef, copySRef
  , runRP, forkRP, joinRP, synchronizeRP, threadDelayRP, readRP, writeRP
  ) where

import Control.Applicative ((<*>))
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar(..), modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Monad (ap, forM_, liftM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Data.Atomics (loadLoadBarrier, storeLoadBarrier, writeBarrier)
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (delete)
import Debug.Trace (trace)

type Counter = IORef Int64

offline    = 0
online     = 1
counterInc = 2

writeCounter :: Counter -> Int64 -> IO ()
writeCounter c x = x `seq` writeIORef c x

readCounter :: Counter -> IO Int64
readCounter c = do x <- readIORef c
                   x `seq` return x

newCounter :: IO Counter
newCounter  = newIORef online

newGCounter :: IO Counter
newGCounter = newIORef online

-- TODO: add a private online/offline flag that threads update when they go
-- online/offline and no other thread touches, maybe this will improve cache
-- behavior?

data RPState  = RPState  { countersVRP   :: MVar [Counter]  
                         , gCounterRP    :: Counter
                         , writerLockRP  :: MVar () }       

data RPEState = RPEState { countersV  :: MVar [Counter]
                         , gCounter   :: Counter
                         , counter    :: Counter
                         , writerLock :: MVar () }

data ThreadState s a = ThreadState { ctr :: Counter
                                   , rv  :: MVar a
                                   , tid :: ThreadId }

-- Relativistic programming monads
newtype RPIO  s a = UnsafeRPIO  { runRPIO   :: IO a } 
newtype RPEIO s a = UnsafeRPEIO { runRPEIO  :: IO a }
newtype RPRIO s a = UnsafeRPRIO { runRPRIO  :: IO a }
newtype RPWIO s a = UnsafeRPWIO { runRPWIO  :: IO a }

instance Monad (RPIO s) where
  return = UnsafeRPIO  . return
  (UnsafeRPIO  m) >>= k = UnsafeRPIO  $ m >>= runRPIO  . k
instance Applicative (RPIO s) where
  pure  = return
  (<*>) = ap
instance Functor (RPIO s) where
  fmap  = liftM

instance Monad (RPEIO s) where
  return = UnsafeRPEIO . return
  (UnsafeRPEIO m) >>= k = UnsafeRPEIO $ m >>= runRPEIO . k
instance Applicative (RPEIO s) where
  pure  = return
  (<*>) = ap
instance Functor (RPEIO s) where
  fmap  = liftM

instance Monad (RPRIO s) where
  return = UnsafeRPRIO . return
  (UnsafeRPRIO m) >>= k = UnsafeRPRIO $ m >>= runRPRIO . k
instance Applicative (RPRIO s) where
  pure  = return
  (<*>) = ap
instance Functor (RPRIO s) where
  fmap  = liftM

instance Monad (RPWIO s) where
  return = UnsafeRPWIO . return
  (UnsafeRPWIO m) >>= k = UnsafeRPWIO $ m >>= runRPWIO . k
instance Applicative (RPWIO s) where
  pure  = return
  (<*>) = ap
instance Functor (RPWIO s) where
  fmap  = liftM

-- have to use newtypes here since you can't use partially applied
-- type synonyms in instance declarations.
newtype RP  s a = RP  { unRP  :: ReaderT RPState  (RPIO  s) a }
newtype RPE s a = RPE { unRPE :: ReaderT RPEState (RPEIO s) a }
newtype RPR s a = RPR { unRPR ::                  (RPRIO s) a }
newtype RPW s a = RPW { unRPW :: ReaderT RPEState (RPWIO s) a }

instance Monad (RP s) where
  return  = RP  . return
  m >>= f = RP  $ unRP  m >>= unRP  . f 
instance Applicative (RP s) where
  pure    = return
  (<*>)   = ap
instance Functor (RP s) where
  fmap    = liftM
instance Monad (RPE s) where
  return  = RPE . return
  m >>= f = RPE $ unRPE m >>= unRPE . f 
instance Applicative (RPE s) where
  pure    = return
  (<*>)   = ap
instance Functor (RPE s) where
  fmap    = liftM
instance Monad (RPR s) where
  return  = RPR . return
  m >>= f = RPR $ unRPR m >>= unRPR . f 
instance Applicative (RPR s) where
  pure    = return
  (<*>)   = ap
instance Functor (RPR s) where
  fmap    = liftM
instance Monad (RPW s) where
  return  = RPW . return
  m >>= f = RPW $ unRPW m >>= unRPW . f 
instance Applicative (RPW s) where
  pure    = return
  (<*>)   = ap
instance Functor (RPW s) where
  fmap    = liftM

-- Shared references

newtype SRef s a = SRef (IORef a)

class RPRead m s where
  -- | Dereference a cell.
  readSRef :: SRef s a -> m s a

instance RPRead RPR s where
  readSRef = RPR .        UnsafeRPRIO . readSRefIO 

instance RPRead RPW s where
  readSRef = RPW . lift . UnsafeRPWIO . readSRefIO 

readSRefIO :: SRef s a -> IO a
readSRefIO (SRef r) = do x <- readIORef r
                         x `seq` return x

class RPNew m where
  -- | Allocate a new shared reference cell.
  newSRef :: a -> m s (SRef s a)

newSRefIO :: a -> IO (SRef s a)
newSRefIO x = do
  r <- newIORef x
  return $ SRef r

instance RPNew RP where
  newSRef = RP  . lift . UnsafeRPIO  . newSRefIO
instance RPNew RPW where
  newSRef = RPW . lift . UnsafeRPWIO . newSRefIO

copySRefIO :: SRef s a -> IO (SRef s a)
copySRefIO (SRef r) = do
  x  <- readIORef r
  r' <- x `seq` newIORef x -- does this duplicate x?
  return $ SRef r'

copySRef :: SRef s a -> RPW s (SRef s a)
copySRef = RPW . lift . UnsafeRPWIO . copySRefIO

-- | Swap the new version into the reference cell.
writeSRef :: SRef s a -> a -> RPW s ()
writeSRef r x = updateSRef r $ const x
--writeSRef (SRef r) x = UnsafeRPW $ writeIORef r x

-- | Compute an update and swap it into the reference cell.
updateSRef :: SRef s a -> (a -> a) -> RPW s ()
updateSRef (SRef r) f = RPW $ lift $ UnsafeRPWIO $ do
  x <- return . f =<< readIORef r
  x `seq` writeBarrier -- make sure x is evaluated (in particular, 
                       -- that its field writes are issued) before
                       -- we publish a pointer to it.
  writeIORef r x

{- urcu does it like this:
 -  void *rcu_set_pointer_sym(void **p, void *v)
 -  {
 -    cmm_wmb();
 -    return uatomic_set(p, v);
 -  }
 -
 - where uatomic_set(p, v) expands to: 
 - (*(volatile typeof(p) *)&(p)) = (v)
 - 
 - Because field assignments are explicit and effectively in IO in C, this barrier 
 - (which works both at the hardware and compiler level) definitely comes between the
 - writes that populate the fields and the write that publishes the pointer.
 - If x isn't evaluated before it's written to r, is it possible that the writes that populate its
 - fields could be reordered with the write that publishes the pointer to the evaluated version?
 - At least, is it possible on an architecture that reorders stores?
 - Does GHC guarantee that when a thunk is evaluated, the pointer publication is a release write?
 - Does GHC guarantee that when a boxed data object is loaded, the pointer load is an acquire or consume read?
 - It seems very unlikely that GHC includes a memory barrier in either case.
 -}

-- Relativistic computations.

-- | Relativistic computation.
runRP :: (forall s. RP s a) -> IO a
runRP m = do
  gc <- newGCounter
  cv <- newMVar []
  wl <- newMVar ()
  let s = RPState { gCounterRP = gc, countersVRP = cv, writerLockRP = wl }
  runRPIO $ flip runReaderT s $ unRP m

-- | Initialize and run a new relativistic program thread.
--   * Create an MVar for the return value.
--   * Create a counter for grace period tracking.
--   * Spawn the thread.
--   * Return the counter, return MVar, and thread ID.
forkRP :: RPE s a -> RP s (ThreadState s a)
forkRP m = RP $ do 
  c    <- lift $ UnsafeRPIO $ newCounter
  RPState {countersVRP, gCounterRP, writerLockRP} <- ask
  -- add this thread's grace period counter to the global list
  lift $ UnsafeRPIO $ modifyMVar_ countersVRP $ \cs -> do
    return $ c : cs
  v    <- lift $ UnsafeRPIO $ newEmptyMVar -- no return value yet
  let s = RPEState { counter = c, gCounter = gCounterRP
                   , countersV = countersVRP, writerLock = writerLockRP }
  tid  <- lift $ UnsafeRPIO $ forkIO $ 
    do putMVar v =<< (runRPEIO $ flip runReaderT s $ unRPE m)
       -- in case a synchronizeRP caller already holds a reference to ctr 
       -- (and could get stuck waiting for an update to it that will never come).
       -- don't do this inside modifyMVar_ below since that could lead to deadlock
       -- (writer holds MVar, is waiting for all counters to catch up or go offline,
       -- main thread is blocking on that MVar to announce that joined thread has
       -- gone offline).

       -- do I need this barrier?
       writeBarrier
       trace ("thread going offline") $ 
         writeCounter c offline
       -- lock on MVar should act as a barrier
       modifyMVar_ countersVRP $ \cs -> do
         return $ delete c cs 
       return ()

  return $ ThreadState { rv = v, tid = tid, ctr = c }

joinRP :: ThreadState s a -> RP s a
joinRP (ThreadState {tid, rv, ctr}) = RP $ do
  v <- lift $ UnsafeRPIO $ takeMVar rv         -- wait for thread to complete.
  return v

-- | Read-side critical section.
readRP :: RPR s a -> RPE s a
readRP m = RPE $ do
  RPEState {counter, gCounter} <- ask
  -- need to deal with the possibility that this thread was offline; if so, take a snapshot of gCounter.
  lift $ UnsafeRPEIO $ do
    -- do I need this to prevent earlier writes to counter from being reordered below this read?
    -- seems like I should not, since there is a data dependency, and nobody but this thread will
    -- ever write to this counter.
    --writeBarrier     
    c <- readCounter counter
    when (c == offline) $ trace ("reader going online") $ do
      writeCounter counter =<< readCounter gCounter
      storeLoadBarrier -- need a guarantee that this thread will be seen as online before any of
                       -- the reads in the read-side critical section below, right?
  -- run read-side critical section
  x <- lift $ UnsafeRPEIO $ runRPRIO $ unRPR m
  -- for now, just announce a quiescent state at the end of every read-side critical section
  lift $ UnsafeRPEIO $ do 
    storeLoadBarrier
    writeCounter counter =<< readCounter gCounter
    storeLoadBarrier
  return x

-- | Write-side critical section.
writeRP :: RPW s a -> RPE s a
writeRP m = RPE $ do 
  s@(RPEState {writerLock}) <- ask
  -- run write-side critical section
  -- acquire a coarse-grained lock (separate from counter lock) to serialize writers
  lift $ UnsafeRPEIO $ takeMVar writerLock
  x <- lift $ UnsafeRPEIO $ runRPWIO $ flip runReaderT s $ unRPW $ do
    x <- m
    -- TODO: what if the RPW critical section already ends with a call to
    -- synchronizeRP?  
    -- wait at the end so to guarantee that this critical section
    -- happens before the next critical section.
    synchronizeRP
    return x
  -- release writer-serializing lock
  lift $ UnsafeRPEIO $ putMVar writerLock ()
  -- return critical section's return value
  return x

synchronizeRP :: RPW s ()
synchronizeRP = RPW $ do
  -- wait for readers
  RPEState {counter, gCounter, countersV} <- ask
  c <- lift $ UnsafeRPWIO $ readCounter counter
  lift $ UnsafeRPWIO $ do
    storeLoadBarrier
    when (c /= offline) $ trace ("top setting writer counter offline") $ writeCounter counter offline
  gc' <- lift $ UnsafeRPWIO $ withMVar countersV $ \counters -> do
    gc' <- return . (+ counterInc) =<< readCounter gCounter
    writeCounter gCounter gc'
    writeBarrier
    trace ("waiting for " ++ show (length counters) ++ " counters") $ return ()
    let waitForReader counter = do
          c <- readCounter counter
          trace ("c is " ++ show c ++ ", gc' is " ++ show gc') $ return ()
          if c /= offline && c < gc'
             then do threadDelay 10
                     waitForReader counter
             else trace ("completed wait for reader, c is " ++ show c ++ ", gc' is " ++ show gc') $ return ()
    forM_ counters $ waitForReader
    return gc'
  lift $ UnsafeRPWIO $ do
    when (c /= offline) $ trace ("bottom setting writer counter to gc'") $ writeCounter counter gc'
    storeLoadBarrier
  trace ("completed synchronizeRP") $ return ()

-- | Delay an RP thread.
threadDelayRP :: Int -> RP s ()
threadDelayRP = RP . lift . UnsafeRPIO . threadDelay
