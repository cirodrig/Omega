
module Data.Presburger.Omega.Rel where

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L

-- | Relations, which represent partial functions from Z^m to Z^n.
-- The dimension m of the input is given by 'relDomain'.
-- The output is 'relOut'.

-- Variables are referenced by de Bruijn index.
data Rel = Rel
    { relDimension :: !Int      -- ^ number of variables in the input
    , relOut       :: [Exp Int] -- ^ the function from input to output
    , relDomain    :: Exp Bool  -- ^ domain on which the relation is defined
    }
    deriving(Show)

-- Convert a relation to an omega relation
relToOmegaRel :: Rel -> IO L.OmegaRel
relToOmegaRel r = L.newOmegaRel (relDimension r) rangeVars $ \dom rng ->
    expToFormula (dom ++ rng) constraints
    where
      -- The relation is a function from input variables to a tuple of
      -- integers.  In the low-level interface a relation is just a kind of
      -- set.  We have to convert a tuple [e1, e2, e3] into
      -- some variabls x, y, z and constraints (x = e1 && y = e2 && z = e3).
      rangeExps = relOut r
      rangeVars = length rangeExps
      rangeIndices =
          let base = relDimension r
          in  [base + rangeVars, base + rangeVars - 1, base + 1]

      rangeConstraints = zipWith mkRangeConstraint rangeIndices rangeExps

      mkRangeConstraint n e =
          let rangeVar = Bound n
              term = simplify $ CAUE Sum 0 [e, CAUE Prod (-1) [VarE rangeVar]]
          in PredE IsZero term

      constraints = CAUE Conj True (relDomain r : rangeConstraints)
