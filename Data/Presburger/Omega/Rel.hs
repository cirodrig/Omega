
module Data.Presburger.Omega.Rel
    (Rel,
     -- * Building relations
     rel, functionalRel, fromOmegaRel,
     -- * Operatoins on relations
     inputDimension, outputDimension,
     domain,
     predicate
    )
where

import System.IO.Unsafe

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L
import Data.Presburger.Omega.LowLevel(OmegaRel)
import Data.Presburger.Omega.SetRel
import qualified Data.Presburger.Omega.Set as Set

-- | Partial functions from Z^m to Z^n.

-- Variables are referenced by de Bruijn index.  The order is:
-- [dom_1, dom_2 ... dom_n, rng_1, rng_2 ... rng_m]
-- where rng_1 has the lowest index and dom_m the highest.
data Rel = Rel
    { relInpDim :: !Int         -- ^ number of variables in the input
    , relOutDim :: !Int         -- ^ the function from input to output
    , relFun    :: BoolExp      -- ^ function defining the relation
    , relOmegaRel :: OmegaRel   -- ^ low-level representation of this relation
    }

instance Show Rel where
    showsPrec n r = showParen (n >= 10) $
                    showString "rel " .
                    shows (relInpDim r) .
                    showChar ' ' .
                    shows (relOutDim r) .
                    showChar ' ' .
                    showsPrec 10 (relFun r)
        where
          showChar c = (c:)

-- | Create a new relation from Z^m to Z^n from a predicate that
-- defines the relation.
--
-- The expression should have @m+n@ free variables.  The first @m@
-- variables refer to the domain, and the remaining variables refer to
-- the range.
--
-- Relations should be functional, that is, each element of the domain
-- maps to at most one element of the range.  This property is not
-- verified.

rel :: Int                      -- ^ Dimensionality of the domain
    -> Int                      -- ^ Dimensionality of the range
    -> BoolExp                  -- ^ Predicate defining the relation
    -> Rel
rel inDim outDim expr
    | variablesWithinRange (inDim + outDim) expr =
        Rel
        { relInpDim   = inDim
        , relOutDim   = outDim
        , relFun      = expr
        , relOmegaRel = unsafePerformIO $ mkOmegaRel inDim outDim expr
        }
    | otherwise = error "rel: Variables out of range"

mkOmegaRel inDim outDim expr =
    L.newOmegaRel inDim outDim $ \dom rng -> expToFormula (dom ++ rng) expr

-- | Create a new relation from Z^m to Z^n from functions defining
-- each output in terms of the inputs.
--
-- The expression parameters should have @m@ free variables.
--
-- For example, the relation @{(x, y) -> (y, x) | x > 0 && y > 0}@ is
--
-- > let [x, y] = takeFreeVariables' 2
-- > in functionalRel 2 [y, x] (conjE [y |>| intE 0, x |>| intE 0])

functionalRel :: Int            -- ^ Dimensionality of the domain
              -> [IntExp]       -- ^ Function relating domain to range
              -> BoolExp        -- ^ Predicate restricting the domain
              -> Rel
functionalRel dim range domain
    | all (variablesWithinRange dim) range &&
      variablesWithinRange dim domain =
        Rel
        { relInpDim   = dim
        , relOutDim   = length range
        , relFun      = relationPredicate
        , relOmegaRel = unsafePerformIO $
                        mkFunctionalOmegaRel dim range domain
        }
    | otherwise = error "functionalRel: Variables out of range"
    where
      -- construct the expression domain && rangeVar1 == rangeExp1 && ...
      relationPredicate =
          conjE (domain : zipWith outputPredicate [dim..] range)

      outputPredicate index expr =
          varE (nthVariable index) |==| expr

-- To make an omega relation, we combine the range variables and the domain
-- into one big happy formula, with the conjunction
-- @domain /\ rangeVar1 == rangeExp1 /\ ... /\ rangeVarN == rangeExpN@.

mkFunctionalOmegaRel :: Int -> [IntExp] -> BoolExp -> IO OmegaRel
mkFunctionalOmegaRel dim range domain =
    L.newOmegaRel dim (length range) $ \dom rng ->
        L.conjunction (domainConstraint dom : rangeConstraints dom rng)
    where
      domainConstraint dom = expToFormula dom domain

      rangeConstraints dom rng = zipWith (rangeConstraint dom) range rng

      -- To make a range constraint, we first add the range variable
      -- as the outermost bound variable, then convert this expression to an
      -- equality constraint (rangeVar == ...), then convert 
      rangeConstraint dom expr rngVar =
          let -- Add the range variable as the outermost bound variable
              vars = dom ++ [rngVar]

              -- Turn the range formula into an equality constraint
              -- (rngVar == ...)
              expr' = expr |==| varE (nthVariable dim)

          in expToFormula vars expr'

-- | Convert an 'OmegaRel' to a 'Rel'.
fromOmegaRel :: OmegaRel -> IO Rel
fromOmegaRel orel = do
  (dim, range, expr) <- relToExpression orel
  return $ Rel
             { relInpDim   = dim
             , relOutDim   = range
             , relFun      = expr
             , relOmegaRel = orel
             }

-------------------------------------------------------------------------------
-- Operations on relations

-- Some helper functions
useRel :: (OmegaRel -> IO a) -> Rel -> a
useRel f r = unsafePerformIO $ f $ relOmegaRel r

-- | Get the dimensionality of a relation's domain
inputDimension :: Rel -> Int
inputDimension = relInpDim

-- | Get the dimensionality of a relation's range
outputDimension :: Rel -> Int
outputDimension = relOutDim

-- | Get the predicate defining a relation.
predicate :: Rel -> BoolExp
predicate = relFun

-- | Get the predicate defining a relation's domain
domain :: Rel -> Set.Set
domain r = useRel (\ptr -> Set.fromOmegaSet =<< L.domain ptr) r

