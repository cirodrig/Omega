
-- | Sets whose members are represented compactly using a
-- Presburger arithmetic formula.  This is a high-level interface to
-- 'OmegaSet'.
--
-- This module is intended to be imported qualified, e.g.
--
-- > import qualified Data.Presburger.Omega.Set as WSet

module Data.Presburger.Omega.Set
    (Set,

     -- * Building sets
     set, fromOmegaSet,

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

import System.IO.Unsafe

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L
import Data.Presburger.Omega.LowLevel(OmegaSet, Effort(..))
import Data.Presburger.Omega.SetRel

-- | Sets of points in Z^n defined by a formula.
data Set = Set
    { setDim      :: !Int      -- ^ the number of variables
    , setExp      :: BoolExp   -- ^ a predicate defining the set
    , setOmegaSet :: OmegaSet  -- ^ low-level representation of this set
    }

instance Show Set where
    -- Generate a call to 'set'
    showsPrec n s = showParen (n >= 10) $
                    showString "set " .
                    shows (setDim s) .
                    showChar ' ' .
                    showsPrec 10 (setExp s)

-- | Create a set whose members are defined by a predicate.
--
-- The expression should have one free variable for each dimension.
--
-- For example, the set of all points on the plane is
-- 
-- >  set 2 trueE
-- 
-- The set of all points (x, y, z) where x > y + z is
-- 
-- >  set 3 (case takeFreeVariables' 3 of [x,y,z] -> x |>| y |+| z)
--
set :: Int                      -- ^ Number of dimensions
    -> BoolExp                  -- ^ Predicate defining the set
    -> Set
set dim expr
    | variablesWithinRange dim expr =
        Set
        { setDim      = dim
        , setExp      = expr
        , setOmegaSet = unsafePerformIO $ mkOmegaSet dim expr
        }
    | otherwise = error "set: Variables out of range"

mkOmegaSet :: Int -> BoolExp -> IO OmegaSet
mkOmegaSet dim expr = L.newOmegaSet dim (\vars -> expToFormula vars expr)

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

-- | Get the low-level representation of a set
toOmegaSet :: Set -> OmegaSet
toOmegaSet = setOmegaSet

upperBound :: Set -> Set
upperBound s = useSetSet L.upperBound (setDim s) s

lowerBound :: Set -> Set
lowerBound s = useSetSet L.lowerBound (setDim s) s

lowerBoundSatisfiable :: Set -> Bool
lowerBoundSatisfiable = useSet L.lowerBoundSatisfiable

upperBoundSatisfiable :: Set -> Bool
upperBoundSatisfiable = useSet L.upperBoundSatisfiable

obviousTautology :: Set -> Bool
obviousTautology = useSet L.obviousTautology

definiteTautology :: Set -> Bool
definiteTautology = useSet L.definiteTautology

exact :: Set -> Bool
exact = useSet L.exact

inexact :: Set -> Bool
inexact = useSet L.inexact

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

-- | Gist of one set, given another.
-- The sets must have the same dimension
-- (@dimension s1 == dimension s2@), or an error will be raised.
gist :: Effort -> Set -> Set -> Set
gist effort s1 s2 = useSet2Set (L.gist effort) (setDim s1) s1 s2

complement :: Set -> Set
complement s = useSetSet L.complement (setDim s) s

approximate :: Set -> Set
approximate s = useSetSet L.approximate (setDim s) s