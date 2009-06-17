
module Data.Omega.Rel where

import Data.Omega.Expr
import qualified Data.Omega.LowLevel as L

-- Relations, which represent functions from tuples of integers to
-- tuples of integers.
--
-- Variables are referenced by de Bruijn index.
data Rel = Rel
    { domainVars  :: !Int
    , relationOut :: Exp ExpTuple
    , relationConstraint :: Exp Bool
    }
    deriving(Show)

-- Convert a relation to an omega relation
relToOmegaRel :: Rel -> IO L.OmegaRel
relToOmegaRel r = L.newOmegaRel (domainVars r) rangeVars $ \dom rng ->
    expToFormula (dom ++ rng) constraints
    where
      -- The relation is a function from input variables to a tuple of
      -- integers.  In the low-level interface a relation is just a kind of
      -- set.  We have to convert a tuple [e1, e2, e3] into
      -- some variabls x, y, z and constraints (x = e1 && y = e2 && z = e3).
      rangeExps = unzipExpTuple $ relationOut r
      rangeVars = length rangeExps
      rangeIndices =
          let base = domainVars r
          in  [base + rangeVars, base + rangeVars - 1, base + 1]

      rangeConstraints = zipWith mkRangeConstraint rangeIndices rangeExps

      mkRangeConstraint n e =
          let rangeVar = Bound n
              term = simplify $ CAUE Sum 0 [e, CAUE Prod (-1) [VarE rangeVar]]
          in PredE IsZero term

      constraints = CAUE Conj True (relationConstraint r : rangeConstraints)
