
module Data.Presburger.Omega.Set where

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L

-- Sets quantified over some variables.  The variables
-- have de Bruijn indices.
data Set = Set
    { setVars       :: !Int
    , setConstraint :: Exp Bool
    }
    deriving(Show)

-- Convert a set to an omega relation
setToOmegaSet :: Set -> IO L.OmegaSet
setToOmegaSet s =
    L.newOmegaSet (setVars s) (flip expToFormula $ setConstraint s)

