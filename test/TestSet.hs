
module TestSet(setTests) where

import Test.HUnit
import Test.QuickCheck

import Data.Presburger.Omega.Expr
import Data.Presburger.Omega.Internal.Parser
import qualified Data.Presburger.Omega.Set as WSet
import qualified Data.Presburger.Omega.LowLevel as W
import Debug.Trace

test `shouldBe` expect = test `WSet.equal` expect @? error_message
  where
    error_message = "expected: " ++ show expect ++ "\n but got: " ++ show test

-- A satisfiability test that is solved incorrectly by some versions
-- of the Omega library.
--
-- Set s contains no integer points, but it contains rational points.
-- Its complement contains all integer points.
upstreamSatBug =
  let satisfiable = WSet.upperBoundSatisfiable s
      tautology_c = WSet.definiteTautology (WSet.complement s)
  in TestLabel "Upstream sat bug" $
     TestList [test $ satisfiable @?= False,
               test $ tautology_c @?= True]
  where
    s = WSet.set 2 $ \[x,y] -> conjE [ 4 *| varE y |<=| 5 *| varE x |-| intE 1,
                                       8 *| varE y |>=| 9 *| varE x,
                                       varE x |<| intE 5]

-- Test that parameter variables are ordered consistently in a set
simplification =
  TestLabel "Simplification" $
  TestList [show_test s str, show_test s2 s2_s]
  where
    show_test x s = test $ show x @?= s
                            
    s = WSet.set 3 (\ [x, y, z] -> varE x |==| 2 *| varE y)
    str = "set 3 (\\[x, y, z] -> varE x |==| 2 *| varE y)"

    s2 = WSet.set 2 (\[x, y] -> varE x |>| varE y)
    s2_s = "set 2 (\\[x, y] -> varE x |>| varE y)"

readTest = TestLabel "Parsing sets" $
           TestList [ read_test s s_s
                    , read_test s2 s2_s]
  where
    read_test x string = test $ read string `shouldBe` x
    s_s = "set 1 (\\[x] -> trueE)"
    s = WSet.set 1 (\[x] -> trueE)
    
    s2_s = "set 2 (\\[x,y] -> varE x |>| varE y)"
    s2 = WSet.set 2 (\[x, y] -> varE x |>| varE y)

setTests = TestLabel "Set manipulation" $
           TestList [upstreamSatBug, readTest, simplification]
