--{-# LANGUAGE Safe #-}
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)

import RP ( RP, RPE, RPR, RPW, runRP, forkRP, threadDelayRP, readRP, writeRP, joinRP
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

deleteMiddle :: SRef (RPList a) -> RPW ()
deleteMiddle head = do
  (Cons a rn) <- readSRef head
  (Cons _ rm) <- readSRef rn
  writeSRef head $ Cons a rm 

testList :: RP (SRef (RPList Char))
testList = do
  tail <- newSRef Nil
  c1   <- newSRef $ Cons 'C' tail
  c2   <- newSRef $ Cons 'B' c1
  newSRef $ Cons 'A' c2

compactShow :: (Show a, Eq a) => [a] -> String
compactShow xs = intercalate ", " $ map (\xs -> show (length xs) ++ " x " ++ show (head xs)) $ group xs

main :: IO ()
main = do 
  outs <- runRP $ do
    head  <- testList
    -- spawn 8 readers, each records 100000 snapshots of the list
    rts <- replicateM 8 $ forkRP $ readRP $ replicateM 100000 $ reader head
    -- spawn a writer to delete the middle node
    wt  <- forkRP $ writeRP $ deleteMiddle head
    -- wait for the readers to finish and return snapshots
    rvs <- forM rts joinRP
    -- wait for the writer to finish
    joinRP wt
    return rvs
  forM_ (zip [1..] outs) $ \(i, v) -> do 
    putStrLn $ "Thread " ++ show i ++ ": " ++ compactShow v
