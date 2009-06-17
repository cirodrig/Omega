
module Data.Omega.Set where

import Data.Omega.Expr
import qualified Data.Omega.LowLevel as L

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

