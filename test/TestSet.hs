
module TestSet(setTests) where

import Test.HUnit
import Test.QuickCheck

import Data.Presburger.Omega.Expr
import Data.Presburger.Omega.Internal.Parser
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
        in TestList [test $ satisfiable @?= False,
                     test $ tautology_c @?= True]
  where
    s = WSet.set 2 $ \[x,y] -> conjE [ 4 *| varE y |<=| 5 *| varE x |-| intE 1,
                                       8 *| varE y |>=| 9 *| varE x,
                                       varE x |<| intE 5]

readTest = TestLabel "Parsing sets" $
           TestList [ read_test s s_s
                    , read_test s2 s2_s]
  where
    read_test x string = test $ x `WSet.equal` (read string) @? "Not equal"
    s_s = "setFromExp 1 (\\[x] -> trueE)"
    s = WSet.set 1 (\[x] -> trueE)
    
    s2_s = "setFromExp 2 (\\[x,y] -> varE x |>| varE y)"
    s2 = WSet.set 2 (\[x, y] -> varE x |>| varE y)

                            
setTests = TestLabel "Set manipulation" $
           TestList [test1, readTest]
