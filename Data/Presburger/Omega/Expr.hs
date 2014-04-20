
-- | Expressions are the high-level interface for creating Presburger
-- formulae.  As in Presburger arithmetic, expressions can
-- represent addition, subtraction, quantification, inequalities, and boolean
-- operators.
--
-- Expressions allow formulas to be input in a freeform manner.  When
-- converted to a formula with 'expToFormula', they will be simplified to a
-- form that the underlying library can use.
-- Multplication is unrestricted; however, if an
-- expression involves the product of two non-constant terms, it cannot be
-- converted to a formula.
-- 
-- This module handles expressions and converts them to formulas.
-- Sets and relations are managed by the "Data.Presburger.Omega.Set"
-- and "Data.Presburger.Omega.Rel" modules.

{-# OPTIONS_GHC -XBangPatterns
                -XTypeFamilies
                -XEmptyDataDecls
                -XFlexibleInstances
                -XFlexibleContexts
                -XUndecidableInstances #-}
module Data.Presburger.Omega.Expr
    (-- * Expressions
     Exp, IntExp, BoolExp,
     Var,

     -- ** Construction
     nthVariable, takeFreeVariables, takeFreeVariables',
     varE, nthVarE, intE, boolE, trueE, falseE, negateE,
     sumE, prodE, notE, conjE, disjE,
     (|&&|),
     sumOfProductsE,
     (|+|), (|-|), (|*|), (*|),
     isZeroE, isNonnegativeE,
     (|==|), (|/=|), (|>|), (|>=|), (|<|), (|<=|),
     forallE, existsE,

     -- ** Destruction
     foldIntExp, foldBoolExp,

     -- ** Internal data structures
     --
     -- | These are exported to allow other modules to build the low-level
     -- representation of expressions, and avoid the cost of simplifying
     -- expressions.  Normally, the 'Exp' functions are sufficient.
     Expr, IntExpr, BoolExpr,
     PredOp(..),
     Quantifier(..),
     wrapExpr, wrapSimplifiedExpr,
     varExpr, sumOfProductsExpr, conjExpr, disjExpr, testExpr, existsExpr,

     -- ** Operations on expressions
     expEqual,
     expToFormula,

     -- ** Manipulating variables
     rename,
     adjustBindings,
     variablesWithinRange,

     -- ** TEMPORARY: Showing
     showLambdaList,
     emptyShowsEnv,
     showsIntExprPrec,
     showsBoolExprPrec,
     showShowsEnv,
     getSimplifiedExpr
    )
where

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.IntMap as IntMap
import Data.IntMap(IntMap)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Unique
import Debug.Trace
import System.IO.Unsafe

import Data.Presburger.Omega.Internal.Expr
import Data.Presburger.Omega.Internal.ShowExpr
import Data.Presburger.Omega.LowLevel

-------------------------------------------------------------------------------
-- Converting expressions to formulas

-- | Look up a variable in a list.  The variable's position is its
-- de Bruijn index.

lookupVar :: Int -> [VarHandle] -> VarHandle
lookupVar n (v : vars) | n > 0  = lookupVar (n - 1) vars
                       | n == 0 = v
                       | otherwise = error "lookupVar: negative index"

lookupVar _ [] = error "lookupVar: variable index out of range"

-- | Convert a boolean expression to a formula.
--
-- The expression must be a Presburger formula.  In particular, if an
-- expression involves the product of two non-constant terms, it cannot be
-- converted to a formula.  The library
-- internally simplifies expressions to sum-of-products form, so complex
-- expressions are valid as long as each simplified product has at most
-- one variable.

expToFormula :: [VarHandle]     -- ^ Free variables
             -> BoolExp         -- ^ Expression to convert
             -> Formula
expToFormula freeVars e = exprToFormula freeVars (getSimplifiedExpr e)

exprToFormula :: [VarHandle]     -- ^ Free variables
              -> BoolExpr        -- ^ Expression to convert
              -> Formula
exprToFormula freeVars expr =
    case expr
    of CAUE op lit es
           | lit `isUnitOf` op ->
               case op
               of Conj -> conjunction $ map (exprToFormula freeVars) es
                  Disj -> disjunction $ map (exprToFormula freeVars) es
                  _    -> expToFormulaError "unhandled operator"
           | otherwise ->
               -- This boolean literal overrides all other terms
               if lit then true else false

       PredE op e ->
           case sumToConstraint freeVars e
           of (terms, constant) ->
                  case op
                  of IsZero -> equality terms constant
                     IsGEZ  -> inequality terms constant

       NotE e -> negation $ exprToFormula freeVars e

       LitE True  -> true
       LitE False -> false

       QuantE q e -> let body v = exprToFormula (v:freeVars) e
                     in case q
                        of Forall -> qForall body
                           Exists -> qExists body

-- | Convert an integer term to a coefficients for an equality or
-- inequality constraint.
sumToConstraint :: [VarHandle]  -- ^ free variables
                -> IntExpr      -- ^ expression to convert
                -> ([Coefficient], Int)
sumToConstraint freeVars expr =
    case deconstructSum expr
    of Term constant terms -> (map deconstructTerm terms, constant)
    where
      deconstructTerm :: IntExpr -> Coefficient
      deconstructTerm expr =
          case deconstructProduct expr
          of Term n [VarE (Bound i)] -> Coefficient (lookupVar i freeVars) n
             _ -> expToFormulaError "expression is non-affine"

expToFormulaError :: String -> a
expToFormulaError s = error $ "expToFormula: " ++ s

