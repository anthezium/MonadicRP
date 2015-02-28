{-# LANGUAGE Safe #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM_, replicateM)
import Data.List (group, intercalate)

import RP ( RP, RPE, RPR, RPW, runRP, forkRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef )

data RPList a = Nil
              | Cons a (SRef (RPList a))

snapshot :: Int -> RPList Int -> RPR Int
snapshot acc Nil         = return acc
snapshot acc (Cons x rn) = snapshot (x + acc) =<< readSRef rn

reader :: Int -> Int -> SRef (RPList Int) -> RPR Int
reader 0 acc _    = return acc
reader n acc head = do
  acc' <- snapshot acc =<< readSRef head
  reader (n - 1) acc' head

deleteMiddle :: SRef (RPList a) -> RPW ()
deleteMiddle rl = do
  (Cons a rn) <- readSRef rl
  (Cons _ rm) <- readSRef rn
  writeSRef rl $ Cons a rm 

testList :: RP (SRef (RPList Int))
testList = do
  tail <- newSRef Nil
  c1   <- newSRef $ Cons 1     tail
  c2   <- newSRef $ Cons (- 1) c1
  newSRef $ Cons 0 c2

main :: IO ()
main = do 
  (rvtids, wv) <- runRP $ do
    head    <- testList
    -- spawn 8 readers, each records 100000 snapshots of the list
    rvtids  <- replicateM 8 $ forkRP $ readRP $ reader 100000 0 head
    -- spawn a writer to delete the middle node
    --(wv, _) <- forkRP $ writeRP $ deleteMiddle head
    (wv, _) <- forkRP $ writeRP $ return ()
    return (rvtids, wv)
  -- wait for the readers to finish and print snapshots
  forM_ rvtids $ \(rv, tid) -> do 
    v <- takeMVar rv
    putStrLn $ show tid ++ ": " ++ show v
  -- wait for the writer to finish
  takeMVar wv
