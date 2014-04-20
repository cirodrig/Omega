
import Test.HUnit
import System.Exit

import TestExpr
import TestSet

allTests = TestList [exprTests, setTests]

main = do
  counts <- runTestTT allTests
  if errors counts > 0 || failures counts > 0
    then exitFailure
    else exitSuccess