
module TestRel(relTests) where

import Test.HUnit

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.Set as WSet
import qualified Data.Presburger.Omega.Rel as WRel
import qualified Data.Presburger.Omega.LowLevel as W
import Debug.Trace

test `shouldBe` expect = test `WRel.equal` expect @? error_message
  where
    error_message = "expected: " ++ show expect ++ "\n but got: " ++ show test

-- Test relation composition
composition =
  TestList [test $ (r2 `WRel.composition` r1) `shouldBe` r2_of_r1,
            test $ (r1 `WRel.composition` r2) `shouldBe` r1_of_r2]
  where
    r1 = WRel.rel 1 2 (\[z] [x, y] -> varE x |==| varE y |-| varE z)
    r2 = WRel.functionalRel 2 (\[x, y] -> ([varE y], varE x |==| intE 1))

    r2_of_r1 = WRel.functionalRel 1 (\[z] -> ([intE 1 |+| varE z], trueE))
    r1_of_r2 = WRel.rel 2 2 $ \[x1, y1] [x2, y2] ->
      varE x2 |==| varE y2 |-| varE y1 |&&| varE x1 |==| intE 1

simplification = test $ show r @?= s
  where
    r = WRel.rel 2 1 (\[x, y] [z] -> varE z |==| (-2) *| varE x)
    s = "rel 2 1 (\\[x, y] -> (\\[z] -> varE z |==| (-2) *| varE x))"

relTests = TestLabel "Rel manipulation" $
           TestList [composition, simplification]
