{-# LANGUAGE OverloadedLists #-}

module Clrs.DynamicProgramming.RodCuttingSpec
  ( spec
  ) where

import           Clrs.DynamicProgramming.RodCutting (memoizedCutRod)
import qualified Data.Vector.Unboxed                as UV (Vector)
import           Test.Hspec

spec :: Spec
spec =
    describe "memoizedCutRod" $
      it "works for the example in book" $
        memoizedCutRod examplePs 10 `shouldBe` 30

examplePs :: UV.Vector Int
examplePs = [1, 5, 8, 9, 10, 17, 17, 20, 24, 30]
