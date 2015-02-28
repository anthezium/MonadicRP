{-# LANGUAGE NamedFieldPuns, Safe, TupleSections #-}
module RP 
  ( SRef(), RP(), RPE(), RPR(), RPW(),
    newSRef, readSRef, writeSRef,
    runRP, runRPE, runRPR, runRPW,
    forkRP, threadDelayRP, readRP, writeRP
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar(..), newEmptyMVar, putMVar)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

-- Relativistic programming monads
newtype RP  a = UnsafeRP  { runRP  :: IO a } 
newtype RPE a = UnsafeRPE { runRPE :: IO a }
newtype RPR a = UnsafeRPR { runRPR :: IO a }
newtype RPW a = UnsafeRPW { runRPW :: IO a }

instance Monad RP where
  return = UnsafeRP . return
  (UnsafeRP  m) >>= k = UnsafeRP  $ runRP  . k =<< m

instance Monad RPE where
  return = UnsafeRPE . return
  (UnsafeRPE m) >>= k = UnsafeRPE $ runRPE . k =<< m

instance Monad RPR where
  return = UnsafeRPR . return
  (UnsafeRPR m) >>= k = UnsafeRPR $ runRPR . k =<< m

instance Monad RPW where
  return = UnsafeRPW . return
  (UnsafeRPW m) >>= k = UnsafeRPW $ runRPW . k =<< m

newtype SRef a = SRef (IORef a)

class RPRead m where
  -- | Dereference a cell.
  readSRef :: SRef a -> m a

instance RPRead RPR where
  readSRef (SRef r) = UnsafeRPR $ readIORef r

instance RPRead RPW where
  readSRef (SRef r) = UnsafeRPW $ readIORef r 

-- | Allocate a new shared reference cell.
newSRef :: a -> RP (SRef a)
newSRef x = do
  r <- UnsafeRP $ newIORef x
  return $ SRef r

-- | Spawn a new thread.
forkRP :: RPE a -> RP (MVar a, ThreadId)
forkRP m = UnsafeRP $ do 
  v   <- newEmptyMVar 
  tid <- forkIO $ putMVar v =<< runRPE m
  return (v, tid)

-- | Read-side critical section.
readRP :: RPR a -> RPE a
readRP = UnsafeRPE . runRPR

-- | Write-side critical section.
writeRP :: RPW a -> RPE a
writeRP = UnsafeRPE . runRPW

-- | Swap the new version into the reference cell.
writeSRef :: SRef a -> a -> RPW ()
writeSRef r x = updateSRef r $ const x

-- | Compute an update and swap it into the reference cell.
updateSRef :: SRef a -> (a -> a) -> RPW ()
updateSRef (SRef r) f = UnsafeRPW $ do
  atomicModifyIORef' r ((, ()) . f)

-- | Delay an RP thread.
threadDelayRP :: Int -> RP ()
threadDelayRP = UnsafeRP . threadDelay
