{-# LANGUAGE Safe #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM_, replicateM)
import Data.List (group, intercalate)

import RP ( RP, RPE, RPR, RPW, runRP, forkRP, threadDelayRP, readRP, writeRP
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
reader rl = do
  snapshot =<< readSRef rl

deleteMiddle :: SRef (RPList a) -> RPW ()
deleteMiddle rl = do
  (Cons a rn) <- readSRef rl
  (Cons _ rm) <- readSRef rn
  writeSRef rl $ Cons a rm 

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
  (rvtids, wv) <- runRP $ do
    rl      <- testList
    -- spawn 8 readers, each records 100000 snapshots of the list
    rvtids  <- replicateM 8 $ forkRP $ readRP $ replicateM 100000 $ reader rl
    -- spawn a writer to delete the middle node
    (wv, _) <- forkRP $ writeRP $ deleteMiddle rl
    return (rvtids, wv)
  -- wait for the readers to finish and print snapshots
  forM_ rvtids $ \(rv, tid) -> do 
    v <- takeMVar rv
    putStrLn $ show tid ++ ": " ++ compactShow v
  -- wait for the writer to finish
  takeMVar wv
