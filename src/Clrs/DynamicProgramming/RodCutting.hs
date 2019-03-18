{-# LANGUAGE OverloadedLists #-}

module Clrs.DynamicProgramming.RodCutting
  ( memoizedCutRod
  ) where

import qualified Data.IntMap.Strict  as IntMap
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV (Vector)

memoizedCutRod :: UV.Vector Int -- ^p array
               -> Int -- ^n
               -> Int -- ^maximum revenue
memoizedCutRod ps n = evalState (runReaderT (memoizedCutRod' n) ps) mempty

memoizedCutRod' :: Int -> ReaderT (UV.Vector Int) (State (IntMap Int)) Int
memoizedCutRod' n
  | n <= 0 = pure 0
  | otherwise = do
      ps <- ask
      if n == 1
      then pure $ GV.head ps
      else do
        memo <- get
        case memo IntMap.!? n of
          Just x  -> pure x
          Nothing -> do
            let ps' = GV.take n ps
            x <- GV.maximum <$>
                   GV.imapM (\i p -> (+p) <$> memoizedCutRod' (n - 1 - i)) ps'
            modify $ IntMap.insert n x
            pure x
