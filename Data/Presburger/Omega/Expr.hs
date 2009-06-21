
-- | Expressions are the high-level interface to the Omega library.
-- Expressions can be built in a freeform manner; they will be simplified
-- to a form that the underlying library can use.
-- 
-- This module handles expressions and converts them to formulas.  Other
-- modules build sets and relations.


{-# OPTIONS_GHC -fwarn-incomplete-patterns
                -XBangPatterns
                -XTypeFamilies
                -XEmptyDataDecls
                -XFlexibleInstances
                -XFlexibleContexts #-}
module Data.Presburger.Omega.Expr
    (-- * Expressions

     Exp(..), IntExp, BoolExp,
     Var,

     -- ** Constructing expressions
     nthVariable, takeFreeVariables, takeFreeVariables',
     varE, intE, boolE, trueE, falseE, negateE,
     sumE, prodE, conjE, disjE,
     isZeroE, isNonnegativeE,
     (|+|), (|-|),
     (|==|), (|/=|), (|>|), (|>=|), (|<|), (|<=|),
     forallE, existsE,

     -- ** Manipulating expressions
     expEqual,
     simplify,
     expToFormula,

     -- ** Manipulating variables within expressions
     rename,
     adjustBindings,
     variablesWithinRange,

     -- * Other utilities
     CAUOp(..),
     PredOp(..),
     Quantifier(..),
     zero, unit, isZeroOf, isUnitOf,

     Term,
     deconstructSum, rebuildSum,
     deconstructProduct, rebuildProduct
    )
where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.IntMap as IntMap
import Data.IntMap(IntMap)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Unique
import Debug.Trace
import System.IO.Unsafe

import Data.Presburger.Omega.LowLevel

infixl 6 |+|, |-|
infix 4 |>|, |>=|, |<|, |<=|, |==|, |/=|

-- | Integer and boolean-valued expressions.
data Exp t where
    -- Application of a commutative and associative operator
    CAUE :: !(CAUOp t)          -- operator
         -> !t                  -- literal operand
         -> [Exp t]             -- other operands
         -> Exp t

    -- A predicate on an integer expression
    PredE :: !PredOp            -- operator
          -> Exp Int            -- integer operand
          -> Exp Bool

    -- Boolean negation
    NotE :: Exp Bool -> Exp Bool

    -- A literal
    LitE :: !t -> Exp t

    -- A variable.  Only integer-valued variables are permitted.
    VarE :: !Var -> Exp Int

    -- An expression quantified over an integer variable
    QuantE :: !Quantifier -> Exp t -> Exp t

type IntExp = Exp Int
type BoolExp = Exp Bool

-- | Variables.  Variables are represented internally by de Bruijn indices.

-- Variables are represented by a de Bruijn index.  The "innermost" variable
-- is zero, and outer variables have higher indices.

-- The 'Quantified' constructor is used temporarily when building a quantified
-- expression.  It is only seen by 'rename' and 'adjustBindings'.
data Var = Bound {-# UNPACK #-} !Int
         | Quantified !Unique
           deriving(Eq, Ord)

-- | Produce the Nth bound variable.  Zero is the innermost variable index.
nthVariable :: Int -> Var
nthVariable = Bound

-- | Construct a new quantified variable.
newQuantified :: IO Var
newQuantified = do u <- newUnique
                   return (Quantified u)

-- | A commutative and associative operator with a unit.
-- The type parameter 't' gives the operator's parameter and return type.
data CAUOp t where
    Sum  :: CAUOp Int
    Prod :: CAUOp Int
    Conj :: CAUOp Bool 
    Disj :: CAUOp Bool

instance Eq (CAUOp t) where
    Sum  == Sum  = True
    Prod == Prod = True
    Conj == Conj = True
    Disj == Disj = True
    _    == _    = False

instance Show (CAUOp t) where
    show Sum  = "Sum"
    show Prod = "Prod"
    show Conj = "Conj"
    show Disj = "Disj"

-- | A predicate on an integer expresion.
data PredOp = IsZero | IsGEZ
              deriving(Eq, Show)

-- Quantifiers.
data Quantifier = Forall | Exists
                  deriving(Eq, Show)

freeVariables :: [Var]
freeVariables = map Bound [0..]

-- | Produce a set of variables to use as "free variables" in an expression.
-- This produces the list @[nthVariable 0, nthVariable 1, ...]@
takeFreeVariables :: Int -> [Var]
takeFreeVariables n = take n freeVariables

-- | A convenience function that turns each variable in 'takeFreeVariables'
-- into an expression.
takeFreeVariables' :: Int -> [IntExp]
takeFreeVariables' n = map varE $ take n freeVariables

varE :: Var -> IntExp
varE = VarE

intE :: Int -> IntExp
intE = LitE

boolE :: Bool -> BoolExp
boolE = LitE

trueE, falseE :: BoolExp
trueE = boolE True
falseE = boolE False

negateE :: IntExp -> IntExp
negateE e = CAUE Prod (-1) [e]

-- | Sum of integer expressions
sumE :: [IntExp] -> IntExp
sumE = CAUE Sum 0

-- | Product of integer expressions
prodE :: [IntExp] -> IntExp
prodE = CAUE Prod 1

-- | Conjunction of boolean expressions
conjE :: [BoolExp] -> BoolExp
conjE = CAUE Conj True

-- | Disjunction of boolean expressions
disjE :: [BoolExp] -> BoolExp
disjE = CAUE Disj False

-- | Test whether an integer expression is zero
isZeroE :: IntExp -> BoolExp
isZeroE = PredE IsZero

-- | Test whether an integer expression is nonnegative
isNonnegativeE :: IntExp -> BoolExp
isNonnegativeE = PredE IsGEZ 

-- | Add
(|+|) :: IntExp -> IntExp -> IntExp
e |+| f = sumE [e,f]

-- | Subtract
(|-|) :: IntExp -> IntExp -> IntExp
e |-| f = sumE [e, negateE f]

-- | Equality test
(|==|) :: IntExp -> IntExp -> BoolExp
e |==| f = isZeroE (e |-| f)

-- | Inequality test
(|/=|) :: IntExp -> IntExp -> BoolExp
e |/=| f = disjE [e |>| f, e |<| f]

-- | Greater than
(|>|) :: IntExp -> IntExp -> BoolExp
e |>| f = isNonnegativeE (CAUE Sum (-1) [e, negateE f])

-- | Less than
(|<|) :: IntExp -> IntExp -> BoolExp
e |<| f = f |>| e

-- | Greater than or equal
(|>=|) :: IntExp -> IntExp -> BoolExp
e |>=| f = isNonnegativeE (e |-| f)

-- | Less than or equal
(|<=|) :: IntExp -> IntExp -> BoolExp
e |<=| f = f |>=| e

-- | Build a universally quantified formula.
forallE :: (Var -> Exp t) -> Exp t
forallE f = QuantE Forall $ withFreshVariable f

-- | Build an existentially quantified formula.
existsE :: (Var -> Exp t) -> Exp t
existsE f = QuantE Exists $ withFreshVariable f

-- | Use a fresh variable in an expression.  After the expression is
-- constructed, rename/adjust variable indices so that the fresh variable
-- has index 0 and all other free variables' indices are incremented
-- by 1.
withFreshVariable :: (Var -> Exp t) -> Exp t
withFreshVariable f = unsafePerformIO $ do
  v <- newQuantified
  return $ rename v (Bound 0) $ adjustBindings 0 1 $ f v

-------------------------------------------------------------------------------

isLitE :: Exp t -> Bool
isLitE (LitE _) = True
isLitE _        = False

deconstructProduct :: IntExp -> Term Int
deconstructProduct (CAUE Prod n xs) = (n, xs)
deconstructProduct e                = (unit Prod, [e])

rebuildProduct :: Term Int -> Exp Int
rebuildProduct (1, [e]) = e
rebuildProduct (n, es)  = CAUE Prod n es

deconstructSum :: Exp Int -> Term Int
deconstructSum (CAUE Sum n xs) = (n, xs)
deconstructSum e               = (unit Sum, [e])

rebuildSum :: Term Int -> Exp Int
rebuildSum (1, [e]) = e
rebuildSum (n, es)  = CAUE Sum n es

-- Get the 'equality' operator for type t.
cauEq :: CAUOp t -> t -> t -> Bool
cauEq Sum  = (==)
cauEq Prod = (==)
cauEq Conj = (==)
cauEq Disj = (==)

-- Get the 'shows' operator for type t.
cauShows :: CAUOp t -> t -> ShowS
cauShows Sum  = shows
cauShows Prod = shows
cauShows Conj = shows
cauShows Disj = shows

-- Get the zero for a CAU op (if one exists)
zero :: CAUOp t -> Maybe t
zero Sum  = Nothing
zero Prod = Just 0
zero Conj = Just False
zero Disj = Just True

-- Get the unit for a CAU op
unit :: CAUOp t -> t
unit Sum  = 0
unit Prod = 1
unit Conj = True
unit Disj = False

-- | True if the literal is the operator's zero.
isZeroOf :: t -> CAUOp t -> Bool
l `isZeroOf` op = case zero op
                  of Nothing -> False
                     Just z  -> cauEq op l z

-- | True if the literal is the operator's unit.
isUnitOf :: t -> CAUOp t -> Bool
l `isUnitOf` op = cauEq op (unit op) l

-- Evaluate an operator on a list of literals
evalCAUOp :: CAUOp t -> [t] -> t
evalCAUOp Sum  = sum
evalCAUOp Prod = product
evalCAUOp Conj = and
evalCAUOp Disj = or

-- Evaluate a predicate
evalPred :: PredOp -> Int -> Bool
evalPred IsZero = (0 ==)
evalPred IsGEZ  = (0 <=)

-------------------------------------------------------------------------------
-- Show instance for Exp

instance Show (Exp Int) where
    showsPrec _ (CAUE op lit es) =
        showSExpr (shows op : maybeToList (showLiteral op lit) ++ map shows es)

    showsPrec _ (LitE l)     = shows l
    showsPrec _ (VarE v)     = showVar v
    showsPrec _ (QuantE q e) = showQuant q e
    showsPrec _ _            = error "instance Show Exp: unreachable case"

instance Show (Exp Bool) where
    showsPrec _ (CAUE op lit es) =
        showSExpr (shows op : maybeToList (showLiteral op lit) ++ map shows es)

    showsPrec _ (PredE op e) = showSExpr [shows op, shows e]
    showsPrec _ (NotE e)     = showSExpr [showString "Not", shows e]
    showsPrec _ (LitE l)     = shows l
    showsPrec _ (QuantE q e) = showQuant q e
    showsPrec _ _            = error "instance Show Exp: unreachable case"

-- Show a symbolic expression in parentheses
showSExpr :: [ShowS] -> ShowS
showSExpr ss z =
    showChar '(' $ foldr ($) (showChar ')' $ z) (intersperse (showChar ' ') ss)

-- Show a tuple in [,,] syntax
showTuple :: [ShowS] -> ShowS
showTuple ss z =
    showChar '[' $
    foldr ($) (showChar ']' $ z) (intersperse (showString ", ") ss)

-- An ExpTuple value can be shown with the same syntax
showExpTuple :: Show (Exp t) => [Exp t] -> String
showExpTuple xs = showsExpTuple xs []

showsExpTuple :: Show (Exp t) => [Exp t] -> ShowS
showsExpTuple xs = showTuple $ map shows xs

-- Show a quantified expression, e.g. (Forall x. (x + 1))
showQuant :: Show e => Quantifier -> e -> ShowS
showQuant q e = showSExpr [ showString (show q) . showChar '.'
                          , shows e
                          ]

showVar (Bound n)      = showString ("Bound " ++ show n)
showVar (Quantified n) = showString "Quantified"

-- Show a literal but be a litle bit more compact if the literal is redundant.
-- If the literal is the unit for the given CA operator, return Nothing.
-- Otherwise, return a ShowS.
showLiteral :: Show a => CAUOp a -> a -> Maybe ShowS
showLiteral op l
    | l `isUnitOf` op = Nothing
    | otherwise       = Just $ shows l

-------------------------------------------------------------------------------
-- Syntactic equality on expressions

-- | Decide whether two expressions are equal, taking into account
-- commutativity, associativity, and alpha-renaming.
expEqual :: Eq t => Exp t -> Exp t -> Bool
expEqual expr1 expr2 =
    case (expr1, expr2)
    of (CAUE op1 l1 es1, CAUE op2 l2 es2) ->
          op1 == op2 && l1 == l2 && expListsEqual es1 es2

       (PredE op1 e1, PredE op2 e2) ->
          op1 == op2 && expEqual e1 e2

       (NotE e1, NotE e2) -> expEqual e1 e2

       (LitE l1, LitE l2) -> l1 == l2

       (VarE v1, VarE v2) -> v1 == v2

       (QuantE q1 e1, QuantE q2 e2) ->
          q1 == q2 && expEqual e1 e2

       (_, _) -> False          -- Different constructors

-- Decide whether two unordered expression lists are equal.
-- For each element of the first list, find
-- a matching element of the second list and repeat.
expListsEqual :: Eq t => [Exp t] -> [Exp t] -> Bool
expListsEqual (e:es1) es2 =
    case findEqualExp e es2
    of Just (_, es2') -> expListsEqual es1 es2'
       Nothing        -> False

expListsEqual [] [] = True      -- All elements matched
expListsEqual [] _  = False     -- Some leftover elements in es2

findEqualExp :: Eq t => Exp t -> [Exp t] -> Maybe (Exp t, [Exp t])
findEqualExp searchE es = go es id
    where
      go (e:es) h | expEqual searchE e = Just (e, h es)
                  | otherwise          = go es (h . (e:))
      go []     _                      = Nothing

-------------------------------------------------------------------------------
-- Simplification rules

-- This is the main rule for simplifying an expression.
--
-- First, subexpressions are simplified (simplifyRec).
-- Then "basic" simplifications are performed.  These restructure the
-- current term, but no other terms.
-- Then complex simplifications are performed that restructure the current
-- term and subtems.

-- | Normalize an expression.
simplify :: Exp t -> Exp t
simplify e =
    complexSimplifications $ basicSimplifications $ simplifyRec e

simplifyRec :: Exp t -> Exp t
simplifyRec expr =
    case expr
    of CAUE op lit es -> CAUE op lit $ map simplify es
       PredE op e1 -> PredE op $ simplify e1
       NotE e -> NotE $ simplify e
       LitE _ -> expr
       VarE v -> expr
       QuantE q e -> QuantE q $ simplify e 

basicSimplifications :: Exp t -> Exp t
basicSimplifications = zus . peval . flatten

-- Some complex simplifications require steps of simplification to be re-run.
complexSimplifications :: Exp t -> Exp t
complexSimplifications e =
    case e
    of CAUE Sum _ _  -> basicSimplifications $ collect e
       CAUE Prod _ _ -> posToSop e
       _             -> e

-- Convert a product of sums to a sum of products.  If conversion happens,
-- simplification is re-run.

posToSop :: Exp Int -> Exp Int
posToSop expr@(CAUE Prod n es)
    | all (isSingletonList . snd) terms =
        -- If no terms are sums, then the expression is unchanged
        expr

    | otherwise =
          let -- Make a list of lists.
              -- The expression corresponds to
              --   product (map sum terms')
              terms' = [LitE n] : map mkTermList terms

              -- The cartesian product converts this to a sum of products.
              sop    = sequence terms'
              expr'  = CAUE Sum 0 (map (CAUE Prod 1) sop)
          in simplify expr'
    where
      terms = map deconstructSum es
      mkTermList (n, es) = LitE n : es
      isSingletonList [_] = True
      isSingletonList _   = False

posToSop expr = expr            -- Terms other than products are not modified

-- Flatten nested CA expressions
flatten :: forall t. Exp t -> Exp t
flatten (CAUE op lit es) = CAUE op lit (flat es)
    where
      -- Wherever a nested CA expression with the same operator appears,
      -- include its terms in the list
      flat :: [Exp t] -> [Exp t]
      flat (e:es) = case e
                    of CAUE op2 lit2 es2
                           | op == op2 -> LitE lit2 : es2 ++ flat es
                       _ -> e:flat es
      flat []     = []
flatten e = e

-- Partially evaluate an expression
peval :: Exp t -> Exp t
peval exp@(CAUE op l es) =
    case partition isLitE es
    of ([], _)         -> exp
       (lits, notLits) -> let literals = l : map fromLitE lits
                          in CAUE op (evalCAUOp op literals) notLits
    where
      fromLitE (LitE l) = l
      fromLitE _        = error "peval: unexpected expression"

peval exp@(PredE op e) =
    case e
    of LitE l -> LitE $ evalPred op l
       _      -> exp

peval exp@(NotE e) =
    case e
    of LitE l -> LitE $ not l
       _      -> exp

peval e = e

-- Zero, unit, singleton rules.  May eliminate an
-- expression here.
zus :: Exp t -> Exp t
zus exp@(CAUE op l es) =
    case es
    of [] -> LitE l
       [e] | l `isZeroOf` op -> LitE l -- zero * x = zero
           | l `isUnitOf` op -> e      -- unit * x = x
           | otherwise       -> exp    -- no simplificaiton
       _ | l `isZeroOf` op   -> LitE l -- zero * x = zero
         | otherwise         -> exp    -- no simplification

zus e = e

-- Given a sum of products, collect terms that differ only in their
-- constant multiplier.
--
-- For example:
--
--  collect (2xy + 3x - 3xy)
--  becomes (-1)xy + 3x

type Term t = (t, [Exp t])

collect :: Exp Int -> Exp Int
collect (CAUE Sum literal es) =
    let es' = map simplify $
              map rebuildProduct $
              collectTerms $
              map deconstructProduct es
    in CAUE Sum literal es'

    where
      collectTerms :: [Term Int] -> [Term Int]
      collectTerms (t:ts) =
          case collectTerm t ts of (t', ts') -> t':collectTerms ts'
      collectTerms [] = []

      -- Collect together all terms from the list that differ from
      -- the first term only in their multiplier.  The collected terms'
      -- multipliers are summed.  The result is the collected term
      -- and the unused terms from the list.
      collectTerm :: Term Int -> [Term Int] -> (Term Int, [Term Int])
      collectTerm (factor, t) terms =
          let (equalTerms, terms') = partition (sameTerms t) terms
              factor'              = factor + sum (map fst equalTerms)
          in ((factor', t), terms')

      -- Decide whether the expression lists are equal.
      sameTerms t (_, t') = expListsEqual t t'

collect e = e                   -- Terms other than sums do not change

-------------------------------------------------------------------------------
-- Converting expressions to formulas

-- | Look up a variable in a list.  The variable's position is its
-- de Bruijn index.

lookupVar :: Int -> [VarHandle] -> VarHandle
lookupVar n (v : vars) | n > 0  = lookupVar (n - 1) vars
                       | n == 0 = v
                       | otherwise = error "lookupVar: negative index"

lookupVar _ [] = error "lookupVar: variable index out of range"

-- | Convert a simplified boolean expression to a formula.
-- This function is only designed to handle expressions that were
-- simplified by 'simplify'.  Other expressions may cause this function
-- to throw an error.

expToFormula :: [VarHandle]     -- ^ Free variables
             -> BoolExp         -- ^ Expression to convert
             -> Formula
expToFormula freeVars expr =
    case expr
    of CAUE op lit es
           | lit `isUnitOf` op ->
               case op
               of Conj -> conjunction $ map (expToFormula freeVars) es
                  Disj -> disjunction $ map (expToFormula freeVars) es
                  _    -> error "expToFormula: unhandled operator"
           | lit `isZeroOf` op ->
               if lit then true else false
           | otherwise ->
               -- This will never happen, but it silences a compiler warning
               error "expToFormula: unexpected boolean literal"

       PredE op e ->
           case sumToConstraint freeVars e
           of (terms, constant) ->
                  case op
                  of IsZero -> equality terms constant
                     IsGEZ  -> inequality terms constant

       NotE e -> negation $ expToFormula freeVars e

       LitE True  -> true
       LitE False -> false

       QuantE q e -> let body v = expToFormula (v:freeVars) e
                     in case q
                        of Forall -> qForall body
                           Exists -> qExists body

       _ -> error "expToFormula: unexpected expression"

-- | Convert an integer term to a formula.

sumToConstraint :: [VarHandle] -> IntExp -> ([Coefficient], Int)
sumToConstraint freeVars expr =
    case deconstructSum expr
    of (constant, terms) -> (map deconstructTerm terms, constant)
    where
      deconstructTerm :: IntExp -> Coefficient
      deconstructTerm expr =
          case deconstructProduct expr
          of (n, [VarE (Bound i)]) -> Coefficient (lookupVar i freeVars) n
             _ -> error "sumToConstraint: cannot convert non-affine \
                        \expression to constraint"

-- Apply a single variable substitution to an expression.
rename :: Var -> Var -> Exp t -> Exp t
rename v1 v2 expr = rn expr
    where
      rn :: forall t. Exp t -> Exp t
      rn (CAUE op lit es) = CAUE op lit $ map rn es
      rn (PredE op e)     = PredE op $ rn e
      rn (NotE e)         = NotE $ rn e
      rn expr@(LitE _)    = expr
      rn expr@(VarE v)    | v == v1   = VarE v2
                          | otherwise = expr
      rn (QuantE q e)     = QuantE q $ rename (bumpIndex v1) (bumpIndex v2) e

      -- Increment a de Bruijn index
      bumpIndex (Bound n)        = Bound (n+1)
      bumpIndex v@(Quantified _) = v

-- | Adjust bound variable bindings by adding 'shift' to all bound variable
-- indices greater than or equal to 'first'.
adjustBindings :: Int           -- ^ first variable to change
               -> Int           -- ^ Amount to shift by
               -> Exp t         -- ^ Input expression
               -> Exp t         -- ^ Adjusted expression
adjustBindings !firstBound !shift e = adj e
    where
      adj :: Exp t -> Exp t
      adj (CAUE op lit es) = CAUE op lit (map adj es)
      adj (PredE op e)     = PredE op (adj e)
      adj (NotE e)         = NotE (adj e)
      adj expr@(LitE _)    = expr
      adj expr@(VarE v)    = case v
                             of Bound n
                                    | n >= firstBound ->
                                        VarE $ Bound (n + shift)
                                    | otherwise ->
                                        expr
                                Quantified _ -> expr
      adj (QuantE q e)     = QuantE q $ adjustBindings (firstBound + 1) shift e

-- | Check whether the expression has no more than the specified number
-- of free variables.
variablesWithinRange :: Int -> Exp t -> Bool
variablesWithinRange n e = check e
    where
      check :: Exp t -> Bool
      check (CAUE _ _ es)         = all check es
      check (PredE _ e)           = check e
      check (NotE e)              = check e
      check (LitE _)              = True
      check (VarE (Bound i))      = i < n
      check (VarE (Quantified _)) = error "Unexpected quantified variable"
      check (QuantE _ e)          = variablesWithinRange (n+1) e