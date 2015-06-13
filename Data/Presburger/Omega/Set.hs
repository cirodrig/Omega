{-| Sets of integer tuples, represented by their characteristic function.

Sets can be constructed from their characteristic function,
given as an 'Exp'.
For instance, @'set' 3 (\\[x,y,z] -> varE x |>| varE y |+| varE z)@
computes the set of all points @(x, y, z)@ where @x > y + z@.

Sets can be queried by transforming them and checking whether the result
is empty ('lowerBoundSatisfiable') or contains every point
('definiteTautology').
For example, to check whether set
@s@ contains any positive integers, one can check whether its intersection
with the set of positive values is nonempty:
@'lowerBoundSatisfiable' (s `intersection` set 1 (\\[x] -> x |>| 0)@.

Some functions cannot always produce a result that is representable as
a Presburger arithmetic formula.  Internally, an \"UNKNOWN\" term is
used for terms that can't be represented precisely.  UNKNOWN terms can't
be constructed directly from expressions.

This module is a high-level interface to 'OmegaSet'.

This module is intended to be imported qualified, e.g.

> import qualified Data.Presburger.Omega.Set as WSet
-}

module Data.Presburger.Omega.Set
    (Set,

     -- * Building sets
     set, setFromExp, fromOmegaSet,

     -- * Operations on sets
     toOmegaSet,

     -- ** Inspecting
     dimension, predicate,
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
     union, intersection, difference,
     Effort(..),
     gist,

     -- ** Unary operations
     complement,
     approximate
    )
where

import Test.QuickCheck.Arbitrary
import System.IO.Unsafe

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L
import Data.Presburger.Omega.LowLevel(OmegaSet, Effort(..))
import Data.Presburger.Omega.SetRel
import Data.Presburger.Omega.Internal.Arbitrary
import Data.Presburger.Omega.Internal.Expr
import Data.Presburger.Omega.Internal.ShowExpr
import Data.Presburger.Omega.Internal.ShowUtil

-- de Bruijn numbering of a set's parameters
--
-- A set in [dom_1 ... dom_n]
-- would be written by the user as
--
-- > set n $ \[dom1, ..., dom_n] -> ...
--
-- Following the convention that the innermost (rightmost) variable has
-- the lowest index, the parameters are numbered in reverse order:
-- [dom1 = n-1, dom2 = n-2, ..., dom_n = 0].

-- | A set of points in Z^n defined by a characteristic function.
data Set = Set
    { setDim      :: !Int      -- ^ the number of variables
    , setExp      :: BoolExp   -- ^ a predicate defining the set
    , setOmegaSet :: OmegaSet  -- ^ low-level representation of this set
    }

instance Show Set where
    -- Generate a call to 'set'
    showsPrec n s = showsPrecExpr (showSet s) n

showSet s = showTerminal "set" `showApp`
            showInt (setDim s) `showApp`
            (showLambdaBound (setDim s) $
             showBoolExpr (getSimplifiedExpr $ setExp s))

-- | @set n p@ is the subset of @Z^n@ whose members satisfy predicate @p@.
--
-- For example, the set of all points on the plane is
-- 
-- >  set 2 (\[_,_] -> trueE)
-- 
-- The set of all points @(x, y, z)@ where @x > y + z@ is
-- 
-- >  set 3 (\[x,y,z] -> varE x |>| varE y |+| varE z)
--
set :: Int                      -- ^ Number of dimensions
    -> ([Var] -> BoolExp)       -- ^ Predicate defining the set
    -> Set
set dim mk_expr =
  let dom_v = reverse $ takeFreeVariables dim -- parameter ordering is reversed
  in setFromExp dim (mk_expr dom_v)

-- | The subset of @Z^n@ satisfying the predicate.  The predicate must only
--   use the first @n@ de Bruijn variables.
setFromExp :: Int -> BoolExp -> Set
setFromExp dim expr
    | variablesWithinRange dim expr =
        Set
        { setDim      = dim
        , setExp      = expr
        , setOmegaSet = unsafePerformIO $ mkOmegaSet dim expr
        }
    | otherwise = error "setFromExp: Variables out of range"

-- | Low-level function to create a set of the points satisfying a given
--   predicate.
mkOmegaSet :: Int -> BoolExp -> IO OmegaSet
mkOmegaSet dim expr = L.newOmegaSet dim (\vars -> expToFormula vars expr)

instance Arbitrary Set where
  arbitrary = do
    dom_size <- arbitrarySizedBoundedIntegral
    expr     <- arbitraryLinearBoolExpr dom_size
    return $ setFromExp dom_size (wrapExpr expr)

-------------------------------------------------------------------------------
-- Creating sets from Omega sets

-- | Convert an 'OmegaSet' to a 'Set'.
fromOmegaSet :: OmegaSet -> IO Set
fromOmegaSet oset = do
  (dim, expr) <- setToExpression oset
  return $ Set
             { setDim      = dim
             , setExp      = expr
             , setOmegaSet = oset
             }

-- | Internal function to convert an 'OmegaSet' to a 'Set', when we know
-- the set's dimension.  This can avoid actually building the expression
-- when all we want is the dimension.
omegaSetToSet :: Int -> OmegaSet -> IO Set
omegaSetToSet dim oset = return $
    Set
    { setDim      = dim
    , setExp      = unsafePerformIO $ do (_, expr) <- setToExpression oset
                                         return expr
    , setOmegaSet = oset
    }

-------------------------------------------------------------------------------
-- Using sets

-- First, some helper functions for applying OmegaSet functions to Sets

useSet :: (OmegaSet -> IO a) -> Set -> a
useSet f s = unsafePerformIO $ f (setOmegaSet s)

useSetSet :: (OmegaSet -> IO OmegaSet) -> Int -> Set -> Set
useSetSet f dim s = unsafePerformIO $ do
  omegaSetToSet dim =<< f (setOmegaSet s)

useSet2 :: (OmegaSet -> OmegaSet -> IO a) -> Set -> Set -> a
useSet2 f s1 s2 = unsafePerformIO $ f (setOmegaSet s1) (setOmegaSet s2)

useSet2Set :: (OmegaSet -> OmegaSet -> IO OmegaSet)
           -> Int
           -> Set
           -> Set
           -> Set
useSet2Set f dim s1 s2 = unsafePerformIO $ do
  omegaSetToSet dim =<< f (setOmegaSet s1) (setOmegaSet s2)

-- | Get the dimensionality of the space a set inhabits
dimension :: Set -> Int
dimension = setDim

-- | Get the predicate defining a set's members
predicate :: Set -> BoolExp
predicate = setExp

-- | Convert a 'Set' to an 'OmegaSet'.
toOmegaSet :: Set -> OmegaSet
toOmegaSet = setOmegaSet

-- | Compute the upper bound of a set by setting all UNKNOWN
--   constraints to true.
upperBound :: Set -> Set
upperBound s = useSetSet L.upperBound (setDim s) s

-- | Compute the lower bound of a set by setting all UNKNOWN
--   constraints to false.
lowerBound :: Set -> Set
lowerBound s = useSetSet L.lowerBound (setDim s) s

-- | True if the set is nonempty (characteristic function is
-- satisfiable), treating unknown elements as members.
lowerBoundSatisfiable :: Set -> Bool
lowerBoundSatisfiable = useSet L.lowerBoundSatisfiable

-- | True if the set is nomempty (characteristic function is
-- satisfiable), treating unknown elements as nonmembers.
upperBoundSatisfiable :: Set -> Bool
upperBoundSatisfiable = useSet L.upperBoundSatisfiable

-- | True if the set can cheaply be determined to contain every point
--   (characteric function is a tautology).
obviousTautology :: Set -> Bool
obviousTautology = useSet L.obviousTautology

-- | True if the set contains every point
--   (characteric function is a tautology).
definiteTautology :: Set -> Bool
definiteTautology = useSet L.definiteTautology

-- | True if the set has no UNKNOWN constraints.
exact :: Set -> Bool
exact = useSet L.exact

-- | True if the set has UNKNOWN constraints.
inexact :: Set -> Bool
inexact = useSet L.inexact

-- | True if the set is completely UNKNOWN.
unknown :: Set -> Bool
unknown = useSet L.unknown

-- | Test whether two sets are equal.
-- The sets must have the same dimension
-- (@dimension s1 == dimension s2@), or an error will be raised.
--
-- The answer is precise if both relations are 'exact'.
-- If either relation is inexact, this function returns @False@.
equal :: Set -> Set -> Bool
equal = useSet2 L.equal

-- | Union of two sets.
-- The sets must have the same dimension
-- (@dimension s1 == dimension s2@), or an error will be raised.
union :: Set -> Set -> Set
union s1 s2 = useSet2Set L.union (setDim s1) s1 s2

-- | Intersection of two sets.
-- The sets must have the same dimension
-- (@dimension s1 == dimension s2@), or an error will be raised.
intersection :: Set -> Set -> Set
intersection s1 s2 = useSet2Set L.intersection (setDim s1) s1 s2

-- | Difference of two sets.
-- The sets must have the same dimension
-- (@dimension s1 == dimension s2@), or an error will be raised.
difference :: Set -> Set -> Set
difference s1 s2 = useSet2Set L.difference (setDim s1) s1 s2

-- | Get the gist of a set, given some background truth.  The
-- gist operator uses heuristics to simplify the set while
-- retaining sufficient information to regenerate the original by
-- re-introducing the background truth.  The sets must have the
-- same dimension.
--
-- The gist satisfies the property
--
-- > x === gist effort x given `intersection` given
gist :: Effort -> Set -> Set -> Set
gist effort s1 s2 = useSet2Set (L.gist effort) (setDim s1) s1 s2

-- | The complement of a set.
--   The complement of @s@ contains exactly the elements that are
--   not in @s@.
complement :: Set -> Set
complement s = useSetSet L.complement (setDim s) s

-- | Approximate a set by allowing all existentially quantified
-- variables to take on rational values.  This allows these variables to be
-- eliminated from the formula.
approximate :: Set -> Set
approximate s = useSetSet L.approximate (setDim s) s
