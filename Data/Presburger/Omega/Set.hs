
module Data.Presburger.Omega.Set where

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L

-- | Sets of points in R^n defined by a formula.

-- The variables have de Bruijn indices.
data Set = Set
    { setDimension  :: !Int     -- ^ The number of variables
    , setConstraint :: Exp Bool -- ^ A predicate defining the set
    }
    deriving(Show)

-- Convert a set to an omega relation
setToOmegaSet :: Set -> IO L.OmegaSet
setToOmegaSet s =
    L.newOmegaSet (setDimension s) (flip expToFormula $ setConstraint s)

