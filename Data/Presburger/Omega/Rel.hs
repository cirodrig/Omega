
-- | Relations whose members are represented compactly using a
-- Presburger arithmetic formula.  This is a high-level interface to
-- 'OmegaRel'.
--
-- This module is intended to be imported qualified, e.g.
--
-- > import qualified Data.Presburger.Omega.Rel as WRel

module Data.Presburger.Omega.Rel
    (Rel,
     -- * Building relations
     rel, functionalRel, fromOmegaRel,
     -- * Operations on relations
     toOmegaRel,
     inputDimension, outputDimension,
     domain, range, predicate,
     lowerBoundSatisfiable,
     upperBoundSatisfiable,
     obviousTautology,
     definiteTautology,
     exact,
     inexact,
     unknown,
     union, intersection, composition
    )
where

import System.IO.Unsafe

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L
import Data.Presburger.Omega.LowLevel(OmegaRel)
import Data.Presburger.Omega.SetRel
import qualified Data.Presburger.Omega.Set as Set

-- | A relation from points in a /domain/ Z^m to points in a /range/ Z^n.
--
-- A relation can be considered just a set of points in Z^(m+n).  However,
-- many functions that operate on relations treat the domain and range
-- differently.

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
    -- Generate a call to 'rel'
    showsPrec n r = showParen (n >= 10) $
                    showString "rel " .
                    shows (relInpDim r) .
                    showChar ' ' .
                    shows (relOutDim r) .
                    showChar ' ' .
                    showsPrec 10 (relFun r)
        where
          showChar c = (c:)

-- | Create a relation whose members are defined by a predicate.
--
-- The expression should have @m+n@ free variables, where @m@ and @n@ are
-- the first two parameters.  The first @m@
-- variables refer to the domain, and the remaining variables refer to
-- the range.

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

-- | Create a relation where each output is a function of the inputs.
--
-- Each expression should have @m@ free variables, where @m@
-- is the first parameter.
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

-- | Internal function to convert an 'OmegaRel' to a 'Rel', when we know
-- the relation's dimensions.
omegaRelToRel :: Int -> Int -> OmegaRel -> IO Rel
omegaRelToRel inpDim outDim orel = return $
    Rel
    { relInpDim   = inpDim
    , relOutDim   = outDim
    , relFun      = unsafePerformIO $ do (_, _, expr) <- relToExpression orel
                                         return $ expr
    , relOmegaRel = orel
    }

-------------------------------------------------------------------------------
-- Operations on relations

-- Some helper functions
useRel :: (OmegaRel -> IO a) -> Rel -> a
useRel f r = unsafePerformIO $ f $ relOmegaRel r

useRel2 :: (OmegaRel -> OmegaRel -> IO a) -> Rel -> Rel -> a
useRel2 f r1 r2 = unsafePerformIO $ f (relOmegaRel r1) (relOmegaRel r2)

useRel2Rel :: (OmegaRel -> OmegaRel -> IO OmegaRel)
           -> Int -> Int -> Rel -> Rel -> Rel
useRel2Rel f inpDim outDim r1 r2 = unsafePerformIO $ do
  omegaRelToRel inpDim outDim =<< f (relOmegaRel r1) (relOmegaRel r2)

-- | Get the dimensionality of a relation's domain
inputDimension :: Rel -> Int
inputDimension = relInpDim

-- | Get the dimensionality of a relation's range
outputDimension :: Rel -> Int
outputDimension = relOutDim

-- | Convert a 'Rel' to an 'OmegaRel'.
toOmegaRel :: Rel -> OmegaRel
toOmegaRel = relOmegaRel

-- | Get the predicate defining a relation.
predicate :: Rel -> BoolExp
predicate = relFun

domain :: Rel -> Set.Set
domain r = useRel (\ptr -> Set.fromOmegaSet =<< L.domain ptr) r

range :: Rel -> Set.Set
range r = useRel (\ptr -> Set.fromOmegaSet =<< L.range ptr) r

lowerBoundSatisfiable :: Rel -> Bool
lowerBoundSatisfiable = useRel L.isLowerBoundSatisfiable

upperBoundSatisfiable :: Rel -> Bool
upperBoundSatisfiable = useRel L.isUpperBoundSatisfiable

obviousTautology :: Rel -> Bool
obviousTautology = useRel L.isObviousTautology

definiteTautology :: Rel -> Bool
definiteTautology = useRel L.isDefiniteTautology

exact :: Rel -> Bool
exact = useRel L.isExact

inexact :: Rel -> Bool
inexact = useRel L.isInexact

unknown :: Rel -> Bool
unknown = useRel L.isUnknown

-- | Intersection of two relations.
-- The relations must have the same dimension
-- (@inputDimension r1 == inputDimension r2 && outputDimension r1 == outputDimension r2@),
-- or an error will be raised.
intersection :: Rel -> Rel -> Rel
intersection s1 s2 =
    useRel2Rel L.intersection (relInpDim s1) (relOutDim s1) s1 s2

-- | Union of two relations.
-- The relations must have the same dimension
-- (@inputDimension r1 == inputDimension r2 && outputDimension r1 == outputDimension r2@),
-- or an error will be raised.
union :: Rel -> Rel -> Rel
union s1 s2 = useRel2Rel L.union (relInpDim s1) (relOutDim s1) s1 s2

-- | Composition of two relations.
-- The second relation's output must be the same size as the first's input
-- (@outputDimension r2 == inputDimension r1@),
-- or an error will be raised.
composition :: Rel -> Rel -> Rel
composition s1 s2 =
    useRel2Rel L.composition (relInpDim s2) (relOutDim s1) s1 s2
