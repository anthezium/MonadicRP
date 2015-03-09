--{-# LANGUAGE NamedFieldPuns, Safe #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)
import Debug.Trace (trace)

import RP ( RP, RPE, RPR, RPW, ThreadState(..), tid, runRP, forkRP, joinRP, synchronizeRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef )

data RPList a = Nil
              | Cons a (SRef (RPList a))

snapshot :: RPList a -> RPR [a]
snapshot Nil         = return []
snapshot (Cons x rn) = do 
  l    <- readSRef rn
  rest <- snapshot l
  return $ x : rest

reader :: Show a => SRef (RPList a) -> RPR [a]
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

moveB :: SRef (RPList a) -> RPW ()
moveB head = do
  (Cons a rb) <- readSRef head
  (Cons b rc) <- readSRef rb
  (Cons c rd) <- readSRef rc
  (Cons d re) <- readSRef rd
  rb'         <- newSRef $ Cons b re
  -- link in a new 'B' after 'D'
  writeSRef rd $ Cons d rb' 
  -- any reader who starts during this grace period 
  -- sees either "ABCDE" or "ABCDBE"
  synchronizeRP
  -- remove the old 'B'
  writeSRef head $ Cons a rc
  -- any reader who starts during this grace period 
  -- sees either "ABCDBE" or "ACDBE" (true?)

main :: IO ()
main = do 
  outs <- runRP $ do
    -- initialize list
    head <- testList
    -- spawn 8 readers, each records 10000 snapshots of the list
    rts  <- replicateM 8 $ forkRP $ replicateM 100000 $ readRP $ reader head
    -- spawn a writer to delete the middle node
    wt   <- forkRP $ writeRP $ moveB head
    --wt <- forkRP $ writeRP $ return ()
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt@(ThreadState {tid}) -> do 
      v <- joinRP rt
      return $ show tid ++ ": " ++ compactShow v
    -- wait for the writer to finish
    joinRP wt
    return outs
  forM_ outs putStrLn
