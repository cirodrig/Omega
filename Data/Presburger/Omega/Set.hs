
module Data.Presburger.Omega.Set
    (Set, dimension, predicate, toOmegaSet,
     -- * Building sets
     set, fromOmegaSet,
     -- * Operations on sets
     lowerBoundSatisfiable,
     upperBoundSatisfiable,
     obviousTautology,
     definiteTautology,
     exact,
     inexact,
     unknown,
     union, intersection
    )
where

import System.IO.Unsafe

import Data.Presburger.Omega.Expr
import qualified Data.Presburger.Omega.LowLevel as L
import Data.Presburger.Omega.LowLevel(OmegaSet)
import Data.Presburger.Omega.SetRel

-- | Sets of points in Z^n defined by a formula.
data Set = Set
    { setDim_      :: !Int      -- ^ the number of variables
    , setExp_      :: BoolExp   -- ^ a predicate defining the set
    , setOmegaSet_ :: OmegaSet  -- ^ low-level representation of this set
    }

instance Show Set where
    showsPrec n s = showParen (n >= 10) $
                    showString "set " .
                    shows (setDim_ s) .
                    showChar ' ' .
                    showsPrec 10 (setExp_ s)

-- | Convert a boolean expression to a set of points in Z^n.
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
        { setDim_      = dim
        , setExp_      = expr
        , setOmegaSet_ = unsafePerformIO $ mkOmegaSet dim expr
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
             { setDim_      = dim
             , setExp_      = expr
             , setOmegaSet_ = oset
             }

-- | Internal function to convert an 'OmegaSet' to a 'Set', when we know
-- the set's dimension.  This can avoid actually building the expression
-- when all we want is the dimension.
omegaSetToSet :: Int -> OmegaSet -> IO Set
omegaSetToSet dim oset = do
  (_, expr) <- setToExpression oset
  return $ Set
             { setDim_      = dim
             , setExp_      = expr
             , setOmegaSet_ = oset
             }

-------------------------------------------------------------------------------
-- Using sets

-- First, some helper functions for applying OmegaSet functions to Sets

useSet :: (OmegaSet -> IO a) -> Set -> a
useSet f s = unsafePerformIO $ f (setOmegaSet_ s)

useSet2 :: (OmegaSet -> OmegaSet -> IO a) -> Set -> Set -> a
useSet2 f s1 s2 = unsafePerformIO $ f (setOmegaSet_ s1) (setOmegaSet_ s2)

useSet2Set :: (OmegaSet -> OmegaSet -> IO OmegaSet)
           -> Int
           -> Set
           -> Set
           -> Set
useSet2Set f dim s1 s2 = unsafePerformIO $ do
  omegaSetToSet dim =<< f (setOmegaSet_ s1) (setOmegaSet_ s2)

-- | Get the dimensionality of the space a set inhabits
dimension :: Set -> Int
dimension = setDim_

-- | Get the predicate defining a set's members
predicate :: Set -> BoolExp
predicate = setExp_

-- | Get the low-level representation of a set
toOmegaSet :: Set -> OmegaSet
toOmegaSet = setOmegaSet_

lowerBoundSatisfiable :: Set -> Bool
lowerBoundSatisfiable = useSet L.isLowerBoundSatisfiable

upperBoundSatisfiable :: Set -> Bool
upperBoundSatisfiable = useSet L.isUpperBoundSatisfiable

obviousTautology :: Set -> Bool
obviousTautology = useSet L.isObviousTautology

definiteTautology :: Set -> Bool
definiteTautology = useSet L.isDefiniteTautology

exact :: Set -> Bool
exact = useSet L.isExact

inexact :: Set -> Bool
inexact = useSet L.isInexact

unknown :: Set -> Bool
unknown = useSet L.isUnknown

-- | Intersection of two sets.
-- The sets must have the same dimension
-- (@setDimension s1 == setDimension s2@), or an error will be raised.
intersection :: Set -> Set -> Set
intersection s1 s2 = useSet2Set L.intersection (setDim_ s1) s1 s2

-- | Union of two sets.
-- The sets must have the same dimension
-- (@setDimension s1 == setDimension s2@), or an error will be raised.
union :: Set -> Set -> Set
union s1 s2 = useSet2Set L.union (setDim_ s1) s1 s2