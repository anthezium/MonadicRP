--{-# LANGUAGE NamedFieldPuns, Safe #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)
import Debug.Trace (trace)

import RP ( RP, RPE, RPR, RPW, ThreadState(..), tid, runRP, forkRP, joinRP, synchronizeRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef, copySRef )

data RPList a = Nil
              | Cons a (SRef (RPList a))

snapshot :: RPList a -> RPR [a]
snapshot Nil         = return []
snapshot (Cons x rn) = do 
  l    <- readSRef rn
  rest <- snapshot l
  return $ x : rest

reader :: SRef (RPList a) -> RPR [a]
reader head = do
  snapshot =<< readSRef head
  --h <- readSRef head
  --l <- snapshot h
  --trace ("reader saw " ++ show l) $ return l

testList :: RP (SRef (RPList Char))
testList = do
  tail <- newSRef Nil
  c4   <- newSRef $ Cons 'E' tail
  c3   <- newSRef $ Cons 'D' c4
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2
  newSRef $ Cons 'A' c1
compactShow :: (Show a, Eq a) => [a] -> String
compactShow xs = intercalate ", " $ map (\xs -> show (length xs) ++ " x " ++ show (head xs)) $ group xs

moveBforward :: SRef (RPList a) -> RPW ()
moveBforward head = do
  (Cons a rb)    <- readSRef head
  (Cons b rc)    <- readSRef rb
  cc@(Cons c rd) <- readSRef rc
  (Cons d re)    <- readSRef rd
  -- duplicate the pointer to E
  re'            <- copySRef re
  -- link in a new B after D
  writeSRef re $ Cons b re'
  -- any reader who starts during this grace period 
  -- sees either "ABCDE" or "ABCDBE" 
  --synchronizeRP -- interaction of write order and traversal order means you don't need this
  -- remove the old 'B'
  writeSRef rb $ cc
  -- any reader who starts during this grace period 
  -- sees either "ABCDBE" or "ACDBE" (true?)

moveDback :: SRef (RPList a) -> RPW ()
moveDback head = do
  (Cons a rb)    <- readSRef head
  -- duplicate pointer to b
  rb'            <- copySRef rb
  (Cons b rc)    <- readSRef rb
  (Cons c rd)    <- readSRef rc
  (Cons d re)    <- readSRef rd
  ee             <- readSRef re
  -- link in a new D after A
  writeSRef rb $ Cons d rb'
  -- any reader who starts during this grace period 
  -- sees either "ABCDE" or "ADBCDE"
  synchronizeRP
  -- unlink the old 'D'
  writeSRef rd $ ee
  -- any reader who starts during this grace period 
  -- sees either "ADBCDE" or "ADBCE" (true?)
main :: IO ()
main = do 
  outs <- runRP $ do
    -- initialize list
    head <- testList
    -- spawn 8 readers, each records 10000 snapshots of the list
    rts  <- replicateM 8 $ forkRP $ replicateM 200000 $ readRP $ reader head
    -- spawn a writer to delete the middle node
    wt   <- forkRP $ writeRP $ moveDback head
    --wt <- forkRP $ writeRP $ return ()
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt@(ThreadState {tid}) -> do 
      v <- joinRP rt
      return $ show tid ++ ": " ++ compactShow v
    -- wait for the writer to finish
    outs `seq` joinRP wt
    return outs
  forM_ outs putStrLn
