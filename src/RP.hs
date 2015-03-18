--{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, Safe, TupleSections #-}
-- TODO: figure out how to trust atomic-primops.  Do I need to build a version of it marked as Trustworthy?
{-# LANGUAGE DisambiguateRecordFields, FlexibleInstances, MagicHash, NamedFieldPuns, TupleSections, TypeSynonymInstances #-}
module RP 
  ( SRef(), RP(), RPE(), RPR(), RPW()
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
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (delete)
import Debug.Trace (trace)
import GHC.Exts (MutVar#(..), Word64#(..), newMutVar#, readMutVar#, writeMutVar#, atomicModifyMutVar#) 
import GHC.Prim (RealWorld(..), State#(..))

-- TODO: unboxed counter (single-element array of Word64) in the style of Data.Atomics.Counter.AtomicCounter (but not with Int)
--type MCounter = MutableByteArray# RealWorld Word64#

--newMCounter :: State# RealWorld -> IO (MCounter)
--newMCounter s# = return $ snd $ newMutVar# 1 s#

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

-- right now really just "values that must be protected with mutual exclusion"
data RPState  = RPState  { countersVRP :: MVar [Counter]  -- this field is mutated
                         , gCounterRP  :: Counter }       -- this field is a reference, so it will never change

data RPEState = RPEState { countersV :: MVar [Counter]
                         , gCounter  :: Counter
                         , counter   :: Counter }

data ThreadState a = ThreadState { ctr :: Counter
                                 , rv  :: MVar a
                                 , tid :: ThreadId }

-- Relativistic programming monads
newtype RPIO  a = UnsafeRPIO  { runRPIO   :: IO a } 
newtype RPEIO a = UnsafeRPEIO { runRPEIO  :: IO a }
newtype RPRIO a = UnsafeRPRIO { runRPRIO  :: IO a }
newtype RPWIO a = UnsafeRPWIO { runRPWIO  :: IO a }

instance Monad RPIO where
  return = UnsafeRPIO  . return
  (UnsafeRPIO  m) >>= k = UnsafeRPIO  $ m >>= runRPIO  . k
instance Applicative RPIO where
  pure  = return
  (<*>) = ap
instance Functor RPIO where
  fmap  = liftM

instance Monad RPEIO where
  return = UnsafeRPEIO . return
  (UnsafeRPEIO m) >>= k = UnsafeRPEIO $ m >>= runRPEIO . k
instance Applicative RPEIO where
  pure  = return
  (<*>) = ap
instance Functor RPEIO where
  fmap  = liftM

instance Monad RPRIO where
  return = UnsafeRPRIO . return
  (UnsafeRPRIO m) >>= k = UnsafeRPRIO $ m >>= runRPRIO . k
instance Applicative RPRIO where
  pure  = return
  (<*>) = ap
instance Functor RPRIO where
  fmap  = liftM

instance Monad RPWIO where
  return = UnsafeRPWIO . return
  (UnsafeRPWIO m) >>= k = UnsafeRPWIO $ m >>= runRPWIO . k
instance Applicative RPWIO where
  pure  = return
  (<*>) = ap
instance Functor RPWIO where
  fmap  = liftM

type RP  a = ReaderT RPState  RPIO  a
type RPE a = ReaderT RPEState RPEIO a
type RPR a = RPRIO a
type RPW a = ReaderT RPEState RPWIO a

-- Shared references

newtype SRef a = SRef (IORef a)

class RPRead m where
  -- | Dereference a cell.
  readSRef :: SRef a -> m a

instance RPRead RPRIO where
  readSRef = UnsafeRPRIO . readSRefIO 

instance RPRead (ReaderT RPEState RPWIO) where
  readSRef = lift . UnsafeRPWIO . readSRefIO 

readSRefIO :: SRef a -> IO a
readSRefIO (SRef r) = do x <- readIORef r
                         x `seq` return x

class RPNew m where
  -- | Allocate a new shared reference cell.
  newSRef :: a -> m (SRef a)

newSRefIO :: a -> IO (SRef a)
newSRefIO x = do
  r <- newIORef x
  return $ SRef r

instance RPNew (ReaderT RPState RPIO) where
  newSRef = lift . UnsafeRPIO . newSRefIO
instance RPNew (ReaderT RPEState RPWIO) where
  newSRef = lift . UnsafeRPWIO . newSRefIO

copySRefIO :: SRef a -> IO (SRef a)
copySRefIO (SRef r) = do
  x  <- readIORef r
  r' <- x `seq` newIORef x -- does this duplicate x?
  return $ SRef r'

copySRef :: SRef a -> RPW (SRef a)
copySRef = lift . UnsafeRPWIO . copySRefIO

-- | Swap the new version into the reference cell.
writeSRef :: SRef a -> a -> RPW ()
writeSRef r x = updateSRef r $ const x
--writeSRef (SRef r) x = UnsafeRPW $ writeIORef r x
-- atomic-primops-0.7 provides storeLoadBarrier, loadLoadBarrier, and writeBarrier.
-- these are implemented in the GHC RTS \cite{SMP.h} as assembly.  
-- on x86_64:
-- storeLoadBarrier compiles to
-- __asm__ __volatile__ ("lock; addq $0,0(%%rsp)" : : : "memory");
-- This adds 0 to a quad-word at the bottom of the stack, a no-op whose destination is a memory location that is cached on the core
-- where the instruction executes.  Prefixing this with \texttt{lock} does two things:
--   * It guarantees that no memory access on this core are reordered before or after the lock-prefixed instruction \cite{IntelMemOrder}.
--   * It guarantees that the operation on the destination memory location (the no-op on the bottom of the stack)
--     is atomic from the perspective of all cores.  However, because this location is already cached here, this core doesn't need to
--     lock the bus to ensure atomicity: it can rely on the cache coherency mechanism for that.  And no other core is likely to load the
--     value at the bottom of this thread's stack, so the cache coherency mechanism effectively never generates bus traffic because of this instruction.
-- So the atomicity guarantee doesn't have an effect on performance, while still providing a memory barrier on this core.
-- loadLoadBarrier compiles to
-- __asm__ __volatile__ ("" : : : "memory");
-- No instruction is needed here since on a given core, loads are not reordered with other loads \cite{IntelMemOrder}.
-- writeBarrier also compiles to
-- __asm__ __volatile__ ("" : : : "memory");
-- No instruction is needed here since on a given core, stores are not reordered with other stores except in limited circumstances \cite{IntelMemOrder}
-- that (presumably) do not apply in the assembly GHC generates.

-- | Compute an update and swap it into the reference cell.
updateSRef :: SRef a -> (a -> a) -> RPW ()
--updateSRef (SRef r) f = UnsafeRPW $ do
--  atomicModifyIORef' r ((, ()) . f)
updateSRef (SRef r) f = lift $ UnsafeRPWIO $ do
  storeLoadBarrier -- probably just need writeBarrier, or no barrier
  modifyIORef' r f
  storeLoadBarrier -- probably just need writeBarrier, or no barrier

-- Relativistic computations.

-- | Relativistic computation.
runRP :: RP a -> IO a
runRP m = do
  gc <- newGCounter
  cv <- newMVar []
  let s = RPState { gCounterRP = gc, countersVRP = cv }
  runRPIO $ runReaderT m s

-- | Initialize and run a new relativistic program thread.
--   * Create an MVar for the return value.
--   * Create a counter for grace period tracking.
--   * Spawn the thread.
--   * Return the counter, return MVar, and thread ID.
forkRP :: RPE a -> RP (ThreadState a)
forkRP m = do 
  c    <- lift $ UnsafeRPIO $ newCounter
  RPState {countersVRP, gCounterRP} <- ask
  -- add this thread's grace period counter to the global list
  lift $ UnsafeRPIO $ modifyMVar_ countersVRP $ \cs -> do
    return $ c : cs
  v    <- lift $ UnsafeRPIO $ newEmptyMVar -- no return value yet
  let s = RPEState { counter = c, gCounter = gCounterRP, countersV = countersVRP }
  tid  <- lift $ UnsafeRPIO $ forkIO $ putMVar v =<< (runRPEIO $ runReaderT m s)
  return $ ThreadState { rv = v, tid = tid, ctr = c }

joinRP :: ThreadState a -> RP a
joinRP (ThreadState {rv, ctr}) = do
  v <- lift $ UnsafeRPIO $ takeMVar rv
  RPState {countersVRP} <- ask
  lift $ UnsafeRPIO $ modifyMVar_ countersVRP $ \cs -> do
    return $ delete ctr cs 
  return v

-- | Read-side critical section.
readRP :: RPR a -> RPE a
readRP m = do
  RPEState {counter, gCounter} <- ask
  x <- lift $ UnsafeRPEIO $ runRPRIO m
  -- for now, just announce a quiescent state at the end of every read-side critical section
  lift $ UnsafeRPEIO $ do 
    -- temporary to make debugging easier
    --threadDelay 1000000
    storeLoadBarrier
    -- these atomicModifyIORef' calls (or those below the copy), seem to have fixed the race
    -- condition.  how to get rid of them?
    -- this one too?  what a hell
    --atomicModifyIORef' gCounter (, ()) 
    -- at least this one matters
    --atomicModifyIORef' counter (, ()) 
    writeCounter counter =<< readCounter gCounter
    -- so does this one (below)
    atomicModifyIORef' gCounter (, ()) 
    -- this one (below) seems to matter
    storeLoadBarrier
  return x

-- | Write-side critical section.
writeRP :: RPW a -> RPE a
writeRP m = do 
  s <- ask
  -- run write-side critical section
  -- TODO: add a lock (separate from counter lock) to serialize writers
  lift $ UnsafeRPEIO $ runRPWIO $ flip runReaderT s $ do
    x <- m
    -- TODO: what if the RPW critical section already ends with a call to synchronizeRP?
    synchronizeRP
    return x

synchronizeRP :: RPW ()
synchronizeRP = do
  -- wait for readers
  RPEState {counter, gCounter, countersV} <- ask
  c <- lift $ UnsafeRPWIO $ readCounter counter
  lift $ UnsafeRPWIO $ do
    storeLoadBarrier
    when (c /= offline) $ trace ("top setting writer counter offline") $ writeCounter counter offline
    -- make sure this counter store isn't reordered inside or after the scan below
    writeBarrier
  lift $ UnsafeRPWIO $ withMVar countersV $ \counters -> do
    modifyIORef' gCounter (+ counterInc)
    writeBarrier
    trace ("waiting for " ++ show (length counters) ++ " counters") $ return ()
    forM_ counters $ waitForReader gCounter
  lift $ UnsafeRPWIO $ do
    when (c /= offline) $ trace ("bottom setting writer counter to gCounter") $ writeCounter counter =<< readCounter gCounter
    storeLoadBarrier
  trace ("completed synchronizeRP") $ return ()
  where waitForReader gCounter counter = do
          --atomicModifyIORef' counter (, ())
          c  <- readCounter counter
          --atomicModifyIORef' gCounter (, ())
          gc <- readCounter gCounter
          trace ("c is " ++ show c ++ ", gc is " ++ show gc) $ return ()
          loadLoadBarrier -- to prevent caching of reads.  will this work for that?
          if c /= offline && c /= gc
             then do threadDelay 10
                     waitForReader gCounter counter
             else trace ("completed wait for reader, c is " ++ show c ++ ", gc is " ++ show gc) $ return ()

-- | Delay an RP thread.
threadDelayRP :: Int -> RP ()
threadDelayRP = lift . UnsafeRPIO . threadDelay
