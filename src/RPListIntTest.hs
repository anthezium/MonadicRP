--{-# LANGUAGE NamedFieldPuns, Safe #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)

import RP ( RP, RPE, RPR, RPW, ThreadState(..), tid, runRP, forkRP, joinRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef )

data RPList s a = Nil
              | Cons a (SRef s (RPList s a))

snapshot :: Int -> RPList s Int -> RPR s Int
snapshot acc Nil         = return acc
snapshot acc (Cons x rn) = snapshot (x + acc) =<< readSRef rn

reader :: Int -> Int -> SRef s (RPList s Int) -> RPR s Int
reader 0 acc _    = return acc
reader n acc head = do
  acc' <- snapshot acc =<< readSRef head
  reader (n - 1) acc' head

deleteMiddle :: SRef s (RPList s a) -> RPW s ()
deleteMiddle rl = do
  (Cons a rn) <- readSRef rl
  (Cons _ rm) <- readSRef rn
  writeSRef rl $ Cons a rm 

testList :: RP s (SRef s (RPList s Int))
testList = do
  tail <- newSRef Nil
  c1   <- newSRef $ Cons (- 1) tail
  c2   <- newSRef $ Cons 1     c1
  newSRef $ Cons 1 c2

main :: IO ()
main = do 
  outs <- runRP $ do
    -- initialize list
    head <- testList
    -- spawn 8 readers, each records 100000 snapshots of the list
    rts <- replicateM 8 $ forkRP $ readRP $ reader 1000000 0 head
    -- spawn a writer to delete the middle node
    wt  <- forkRP $ writeRP $ deleteMiddle head
    --wt <- forkRP $ writeRP $ return ()
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt@(ThreadState {tid}) -> do 
      v <- joinRP rt
      return $ show tid ++ ": " ++ show v
    -- wait for the writer to finish
    joinRP wt
    return outs
  forM_ outs putStrLn
