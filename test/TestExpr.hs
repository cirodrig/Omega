
module TestExpr(exprTests) where

import Test.HUnit
import Test.QuickCheck

import Data.Presburger.Omega.Expr
import Data.Presburger.Omega.Internal.Arbitrary
import Data.Presburger.Omega.Internal.ShowExpr
import Data.Presburger.Omega.Internal.Parser

-- | Test whether two expressions are equal by subtracting them.
--   Equal expressions simplify to 0.
--
--   Note that this relies on algebraic simplification to detect equality,
--   and algebraic simplification won't work on everything.
--   It won't eliminate quantifiers.
assertEqualExps :: IntExp -> IntExp -> Assertion
assertEqualExps e1 e2 = assertBool message $ show (e1 |-| e2) == "intE 0"
  where
    message =
      "Expressions not equal:\n\t" ++ show e1 ++ "\n\t" ++ show e2

sumOfProducts = test $ assertEqualExps sop pos
  where
    [x, y] = takeFreeVariables' 2
    sop = x |*| x |-| x |*| y |-| 2 *| y |*| y
    pos = (x |+| y) |*| (x |-| 2 *| y)

readTest = TestLabel "Parsing expressions" $
           TestList [ read_test e1 e1_s
                    , read_test e2 e2_s]
  where
    read_test x string = test $ assertEqualExps x (read string)

    e1_s = "intE (-100)"
    e1 = intE (-100)

    e2_s = "4 *| varE (nthVariable 0) |+| varE (nthVariable 1) |-| 2 *| varE (nthVariable 2)"
    e2 = 4 *| nthVarE 0 |+| nthVarE 1 |-| 2 *| nthVarE 2

-- Verify that 'read' returns the thing that was shown
showTest = test $ do
  result <- quickCheckResult $
            forAll (fmap wrapExpr $ arbitraryLinearExpr 3) $ \e ->
            show (e |-| read (show e)) == "intE 0"
  case result of {Success {} -> return (); _ -> fail $ output result}

exprTests = TestLabel "Expression manipulation" $ TestList
            [sumOfProducts, readTest, showTest]