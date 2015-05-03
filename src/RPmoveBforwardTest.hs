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
  c3   <- newSRef $ Cons 'D' tail
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
  -- duplicate the reference to D
  rd'            <- copySRef rd
  -- link in a new B after C
  writeSRef rd $ Cons b rd'
  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ABCBD" 
  --synchronizeRP -- interaction of write order and traversal order means you don't need this
  -- remove the old 'B'
  writeSRef rb $ cc
  -- any reader who starts during this grace period 
  -- sees either "ABCBD" or "ACBD" 

moveCback :: SRef (RPList a) -> RPW ()
moveCback head = do
  (Cons a rb)    <- readSRef head
  (Cons b rc)    <- readSRef rb
  -- duplicate pointer to B
  rb'            <- copySRef rb
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- link in a new C after A
  writeSRef rb $ Cons c rb'
  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ACBCD"
  synchronizeRP
  -- unlink the old C
  writeSRef rc $ de
  -- any reader who starts during this grace period 
  -- sees either "ACBCD" or "ACBD" 

moveCbackNoSync :: SRef (RPList a) -> RPW ()
moveCbackNoSync head = do
  (Cons a rb)    <- readSRef head
  (Cons b rc)    <- readSRef rb
  -- duplicate reference to B
  rb'            <- copySRef rb
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- link in a new C after A
  writeSRef rb $ Cons c rb'
  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ACBCD"
  --synchronizeRP -- this operation is NOT safe to omit, 
                  -- because write order and traversal order are the same
  -- unlink the old C
  writeSRef rc $ de
  -- any reader who starts during this grace period 
  -- sees "ABD", "ACBCD", or "ACBD" 

main :: IO ()
main = do 
  outs <- runRP $ do
    -- initialize list
    head <- testList
    -- spawn 8 readers, each records 10000 snapshots of the list
    rts  <- replicateM 8 $ forkRP $ replicateM 400000 $ readRP $ reader head
    -- spawn a writer to delete the middle node
    --wt   <- forkRP $ writeRP $ moveCback head
    --wt   <- forkRP $ writeRP $ moveCbackNoSync head
    wt   <- forkRP $ writeRP $ moveBforward head
    --wt <- forkRP $ writeRP $ return ()
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt@(ThreadState {tid}) -> do 
      v <- joinRP rt
      return $ show tid ++ ": " ++ compactShow v
    -- wait for the writer to finish
    joinRP wt
    return outs
  forM_ outs putStrLn
