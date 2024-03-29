{-| Relations on integer tuples, represented by their characteristic function.

Relations can be constructed from their characteristic function,
given as an 'Exp'.  For example, the relation
@'rel' 1 2 (\\[z] [x, y] -> varE z |+| varE y |==| varE x)@
associates a coordinate z with
the diagonal @(z + y, y)@.  The number 1 gives the dimensionality of the
relation's input domain, and the number 2 gives the dimensionality of the
relation's output domain.

A relation can also be constructed from a function that maps inputs to
outputs, together with a predicate restricting the relation's domain.  For
example, the relation that maps every coordinate @(x, y)@ to @(y, x)@ can
be written @'functionalRel' 2 (\\[x, y] -> ([varE y, varE x], trueE))@.

Relations can be queried by transforming them and checking whether the result
is empty ('lowerBoundSatisfiable') or contains every relation
('definiteTautology').
For example, to check whether the relation @r@ maps any values to themselves,
one can extract the subset of @r@ whose members relate values to themselves
and check whether that subset is nonempty:
@'lowerBoundSatisfiable' (r `intersection` functionalRel 1 1 (\\[x] -> varE x)@.

This module is a high-level interface to 'OmegaRel'.

This module is intended to be imported qualified, e.g.

> import qualified Data.Presburger.Omega.Rel as WRel
-}

module Data.Presburger.Omega.Rel
    (Rel,
     -- * Building relations
     rel, functionalRel, relFromExp, fromOmegaRel,

     -- * Operations on relations
     toOmegaRel,

     -- ** Inspecting
     inputDimension, outputDimension,
     predicate,
     lowerBoundSatisfiable,
     upperBoundSatisfiable,
     obviousTautology,
     definiteTautology,
     exact,
     inexact,
     unknown,
     equal,

     -- ** Bounds
     upperBound, lowerBound,

     -- ** Binary operations
     union, intersection, composition, join,
     restrictDomain, restrictRange,
     difference, crossProduct,
     Effort(..),
     gist,

     -- ** Unary operations
     transitiveClosure,
     domain, range,
     inverse,
     complement,
     deltas,
     approximate
    )
where

import Test.QuickCheck.Arbitrary
import System.IO.Unsafe

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L
import Data.Presburger.Omega.LowLevel(OmegaRel, Effort(..))
import Data.Presburger.Omega.SetRel
import qualified Data.Presburger.Omega.Set as Set
import Data.Presburger.Omega.Set(Set)
import Data.Presburger.Omega.Internal.Arbitrary
import Data.Presburger.Omega.Internal.ShowExpr
import Data.Presburger.Omega.Internal.ShowUtil
import Data.Presburger.Omega.Internal.Expr

-- de Bruijn numbering of a relation's parameters
--
-- A relation from [dom_1 ... dom_n] to [rng_1 to rng_m]
-- would be written by the user as
--
-- > rel n m \[dom1, ..., dom_n] [rng_1, ..., rng_m] -> ....
--
-- Following the convention that the innermost (rightmost) variable has
-- the lowest index, the parameters are numbered in reverse order:
-- [dom1 = m+n-1, dom2 = m+n-2, ..., dom_n = m,
--  rng_1 = m-1, rng_2 = m-2, ..., rng_m = 0].

-- | A relation from points in a /domain/ Z^m to points in a /range/ Z^n,
--   defined by a characteristic function.
--
-- A relation can be considered just a set of points in Z^(m+n).  However,
-- many functions that operate on relations treat the domain and range
-- differently.
data Rel = Rel
    { relInpDim :: !Int         -- ^ number of variables in the input
    , relOutDim :: !Int         -- ^ the function from input to output
    , relFun    :: BoolExp      -- ^ function defining the relation
    , relOmegaRel :: OmegaRel   -- ^ low-level representation of this relation
    }

instance Show Rel where
    -- Generate a call to 'rel'
    showsPrec n r = showsPrecExpr (showRel r) n

showRel r = showTerminal "rel" `showApp`
            showInt (relInpDim r) `showApp`
            showInt (relOutDim r) `showApp`
            (showLambdaBound (relInpDim r) $
             showLambdaBound (relOutDim r) $
             showBoolExpr (getSimplifiedExpr $ relFun r))

-- | @rel d r p@ is a relation from @Z^d@ to @Z^r@, consisting of the
--   elements that satisfy predicate @p@.
--
-- For example, the relation that associates a coordinate z with
-- the diagonal @(z + y, y)@ is
--
-- > rel 1 2 (\[z] [x, y] -> varE z |+| varE y |==| varE x)

rel :: Int                         -- ^ Dimensionality of the domain
    -> Int                         -- ^ Dimensionality of the range
    -> ([Var] -> [Var] -> BoolExp) -- ^ Predicate on the domain and range
                                   --   defining the relation
    -> Rel
rel inDim outDim mk_expr =
  let (in_v, out_v) = takeRelVariables inDim outDim
  in relFromExp inDim outDim $ mk_expr in_v out_v

-- | Get the input and output variables of a relation.
--   Following the numbering convention, the variable order is reversed.
takeRelVariables inDim outDim =
  let (r_out_v, fv') = splitAt outDim freeVariables
      r_in_v         = take inDim fv'
  in (reverse r_in_v, reverse r_out_v)

-- | The relation from @Z^d@ to @Z^r@ satisfying the predicate.  The predicate
--   must only use the first @d+r@ de Bruijn variables.
relFromExp :: Int -> Int -> BoolExp -> Rel
relFromExp inDim outDim expr
    | variablesWithinRange (inDim + outDim) expr =
        Rel
        { relInpDim   = inDim
        , relOutDim   = outDim
        , relFun      = expr
        , relOmegaRel = unsafePerformIO $ mkOmegaRel inDim outDim expr
        }
    | otherwise = error "relFromExp: Variables out of range"

mkOmegaRel inDim outDim expr =
    L.newOmegaRel inDim outDim $ \dom rng ->
      let bindings = rng ++ dom -- Range is innermost
      in expToFormula bindings expr

-- | Create a relation where each output is a function of the inputs.
--
-- The defining function consists of a predicate specifying where the domain
-- is valid, and the output coordinates as a function of the input coordinates.
--
-- For example, the relation @{(x, y) -> (y, x) | x > 0 && y > 0}@ is
--
-- > functionalRel 2 (\[x, y] -> ([varE y, varE x],
-- >                              varE x |>| intE 0 |&&| varE y |>| intE 0))

functionalRel :: Int            -- ^ Dimensionality of the domain
              -> ([Var] -> ([IntExp], BoolExp))
                 -- ^ The output coordinate as a function of the input
                 --   coordinate, and a restriction on the domain
              -> Rel
functionalRel dim define_relation =
  relFromExp dim out_dim relationPredicate
    where
      -- Get the defining expressions
      in_v = reverse $ takeFreeVariables dim
      (mapping_i, domain_predicate_i) = define_relation in_v

      -- Introudce output variables; adjust bindings of input variables
      out_dim = length mapping_i
      mapping = map (adjustBindings 0 out_dim) mapping_i
      domain_predicate = adjustBindings 0 out_dim domain_predicate_i

      -- Build the predicate for this relation
      -- construct the expression domain && rangeVar1 == rangeExp1 && ...
      out_v = reverse $ takeFreeVariables out_dim
      relationPredicate =
        let mapping_predicates = [varE v |==| e | (v, e) <- zip out_v mapping]
        in conjE (domain_predicate : mapping_predicates)

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

instance Arbitrary Rel where
  arbitrary = do
    dom_size <- arbitrarySizedBoundedIntegral
    rng_size <- arbitrarySizedBoundedIntegral
    expr     <- arbitraryLinearBoolExpr (dom_size + rng_size)
    return $ relFromExp dom_size rng_size (wrapExpr expr)

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

useRelRel :: (OmegaRel -> IO OmegaRel) -> Int -> Int -> Rel -> Rel
useRelRel f inpDim outDim r = unsafePerformIO $ do
  omegaRelToRel inpDim outDim =<< f (relOmegaRel r)

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

-- | Get the set of values that are in the relation's domain.
domain :: Rel -> Set
domain r = useRel (\ptr -> Set.fromOmegaSet =<< L.domain ptr) r

-- | Get the set of values that are in the relation's range.
range :: Rel -> Set
range r = useRel (\ptr -> Set.fromOmegaSet =<< L.range ptr) r

-- | True if the relation is nonempty (characteristic function is
-- satisfiable), treating unknown elements as members.
lowerBoundSatisfiable :: Rel -> Bool
lowerBoundSatisfiable = useRel L.lowerBoundSatisfiable

-- | True if the relation is nonempty (characteristic function is
-- satisfiable), treating unknown elements as nonmembers.
upperBoundSatisfiable :: Rel -> Bool
upperBoundSatisfiable = useRel L.upperBoundSatisfiable

-- | True if the relation can cheaply be determined to contain every point
--   (characteric function is a tautology).
obviousTautology :: Rel -> Bool
obviousTautology = useRel L.obviousTautology

-- | True if the relation contains every point
--   (characteric function is a tautology).
definiteTautology :: Rel -> Bool
definiteTautology = useRel L.definiteTautology

-- | True if the relation has no UNKNOWN constraints.
exact :: Rel -> Bool
exact = useRel L.exact

-- | True if the relation has UNKNOWN constraints.
inexact :: Rel -> Bool
inexact = useRel L.inexact

-- | True if the relation is entirely UNKNOWN.
unknown :: Rel -> Bool
unknown = useRel L.unknown

upperBound :: Rel -> Rel
upperBound r = useRelRel L.upperBound (relInpDim r) (relOutDim r) r

lowerBound :: Rel -> Rel
lowerBound r = useRelRel L.lowerBound (relInpDim r) (relOutDim r) r

-- | Test whether two relations are equal.
-- The relations must have the same dimension
-- (@inputDimension r1 == inputDimension r2 && outputDimension r1 == outputDimension r2@),
-- or an error will be raised.
--
-- The answer is precise if both relations are 'exact'.
-- If either relation is inexact, this function returns @False@.
equal :: Rel -> Rel -> Bool
equal = useRel2 L.equal

-- | Union of two relations.
-- The relations must have the same dimension
-- (@inputDimension r1 == inputDimension r2 && outputDimension r1 == outputDimension r2@),
-- or an error will be raised.
union :: Rel -> Rel -> Rel
union s1 s2 = useRel2Rel L.union (relInpDim s1) (relOutDim s1) s1 s2

-- | Intersection of two relations.
-- The relations must have the same dimension
-- (@inputDimension r1 == inputDimension r2 && outputDimension r1 == outputDimension r2@),
-- or an error will be raised.
intersection :: Rel -> Rel -> Rel
intersection s1 s2 =
    useRel2Rel L.intersection (relInpDim s1) (relOutDim s1) s1 s2

-- | Composition of two relations.
-- The second relation's output must be the same size as the first's input
-- (@outputDimension r2 == inputDimension r1@),
-- or an error will be raised.
composition :: Rel -> Rel -> Rel
composition s1 s2 =
    useRel2Rel L.composition (relInpDim s2) (relOutDim s1) s1 s2

-- | Same as 'composition', with the arguments swapped.
join :: Rel -> Rel -> Rel
join r1 r2 = composition r2 r1

-- | Restrict the domain of a relation.
--
-- > domain (restrictDomain r s) === intersection (domain r) s
restrictDomain :: Rel -> Set -> Rel
restrictDomain r s = unsafePerformIO $
  omegaRelToRel (relInpDim r) (relOutDim r) =<<
  L.restrictDomain (relOmegaRel r) (Set.toOmegaSet s)

-- | Restrict the range of a relation.
--
-- > range (restrictRange r s) === intersection (range r) s
restrictRange :: Rel -> Set -> Rel
restrictRange r s = unsafePerformIO $
  omegaRelToRel (relInpDim r) (relOutDim r) =<<
  L.restrictRange (relOmegaRel r) (Set.toOmegaSet s)

-- | Difference of two relations.
-- The relations must have the same dimension
-- (@inputDimension r1 == inputDimension r2 && outputDimension r1 == outputDimension r2@),
-- or an error will be raised.
difference :: Rel -> Rel -> Rel
difference s1 s2 =
    useRel2Rel L.difference (relInpDim s1) (relOutDim s1) s1 s2

-- | Cross product of two sets.
crossProduct :: Set -> Set -> Rel
crossProduct s1 s2 = unsafePerformIO $
  omegaRelToRel (Set.dimension s1) (Set.dimension s2) =<<
  L.crossProduct (Set.toOmegaSet s1) (Set.toOmegaSet s2)

-- | Get the gist of a relation, given some background truth.  The
-- gist operator uses heuristics to simplify the relation while
-- retaining sufficient information to regenerate the original by
-- re-introducing the background truth.  The relations must have the
-- same input dimensions and the same output dimensions.
--
-- The gist satisfies the property
--
-- > x === gist effort x given `intersection` given
gist :: Effort -> Rel -> Rel -> Rel
gist effort r1 r2 =
    useRel2Rel (L.gist effort) (relInpDim r1) (relOutDim r1) r1 r2

-- | Get the transitive closure of a relation.  In some cases, the transitive
-- closure cannot be computed exactly, in which case a lower bound is
-- returned.
transitiveClosure :: Rel -> Rel
transitiveClosure r =
    useRelRel L.transitiveClosure (relInpDim r) (relOutDim r) r

-- | The inverse of a relation.  The inverse is produced by swapping
--   the domain and range.
inverse :: Rel -> Rel
inverse s = useRelRel L.inverse (relOutDim s) (relInpDim s) s

-- | The complement of a relation.
complement :: Rel -> Rel
complement s = useRelRel L.complement (relInpDim s) (relOutDim s) s

deltas :: Rel -> Set
deltas = useRel (\wrel -> Set.fromOmegaSet =<< L.deltas wrel)

-- | Approximate a relation by allowing all existentially quantified
-- variables to take on rational values.  This allows these variables to be
-- eliminated from the formula.
approximate :: Rel -> Rel
approximate s = useRelRel L.approximate (relInpDim s) (relOutDim s) s
