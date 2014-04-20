
module TestSet(setTests) where

import Test.HUnit

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.Set as WSet
import qualified Data.Presburger.Omega.LowLevel as W
import Debug.Trace

-- A satisfiability test that is solved incorrectly by some versions
-- of the Omega library.
--
-- Set s contains no integer points, but it contains rational points.
-- Its complement contains all integer points.
test1 = let satisfiable = WSet.upperBoundSatisfiable s
            tautology_c = WSet.definiteTautology (WSet.complement s)
        in traceShow s $ TestList [TestCase $ satisfiable @?= False,
                     TestCase $ tautology_c @?= True]
  where
    s = WSet.set 2 $ \[x,y] -> conjE [ 4 *| varE y |<=| 5 *| varE x |-| intE 1,
                                       8 *| varE y |>=| 9 *| varE x,
                                       varE x |<| intE 5]



setTests = TestLabel "Set manipulation" $
           TestList [test1]
