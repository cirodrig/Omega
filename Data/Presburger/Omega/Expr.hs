
{-# OPTIONS_GHC -fwarn-incomplete-patterns
                -XTypeFamilies
                -XEmptyDataDecls
                -XFlexibleInstances #-}
module Data.Presburger.Omega.Expr
    (ExpTuple(..),
     CAUOp(..),
     PredOp(..),
     Quantifier(..),
     Var(..),
     Exp(..),
     deconstructSum, rebuildSum,
     deconstructProduct, rebuildProduct,
     zero, unit, isZeroOf, isUnitOf,
     expEqual,
     unzipExpTuple,
     simplify,
     expToFormula,
     rename,
     bindVariables
    )
where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.IntMap as IntMap
import Data.IntMap(IntMap)
import qualified Data.Set as Set
import Data.Set(Set)
import Debug.Trace

import Data.Presburger.Omega.LowLevel

-- The tuple type used by expressions is a newtype of a list type.

newtype ExpTuple = ExpTuple [Int]
    deriving(Eq)

-- Commutative and associative operators with a unit.
-- The type parameter gives the operator's parameter and return type.
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

-- Predicates on integers.
data PredOp = IsZero | IsGEZ
              deriving(Eq, Show)

-- Quantifiers.
data Quantifier = Forall | Exists
                  deriving(Eq, Show)

-- Variables.  Bound variables are denoted by a de Bruijn index.
data Var = Bound {-# UNPACK #-} !Int | Free {-# UNPACK #-} !Int
           deriving(Eq, Ord)

-- Expressions
data Exp t where
    -- Commutative and associative expressions.
    -- The literal part of the expression is factored out.
    CAUE :: !(CAUOp t) -> !t -> [Exp t] -> Exp t

    -- Predicates on integers
    PredE :: !PredOp -> Exp Int -> Exp Bool

    -- Boolean negation
    NotE :: Exp Bool -> Exp Bool

    -- Literals
    LitE :: !t -> Exp t

    -- Variables
    VarE :: !Var -> Exp t

    -- Tuples of integers.  Implemented internally as lists.
    TupleE :: [Exp Int] -> Exp ExpTuple

    -- Expressions quantified over an integer variable.
    QuantE :: !Quantifier -> Exp t -> Exp t

isLitE :: Exp t -> Bool
isLitE (LitE _) = True
isLitE _        = False

deconstructProduct :: Exp Int -> Term Int
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

-- Check whether a literal is the zero of an operator
l `isZeroOf` op = case zero op
                  of Nothing -> False
                     Just z  -> cauEq op l z

-- Check whether a literal is the unit of an operator
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
    showsPrec _ (VarE v)     = showVar v
    showsPrec _ (QuantE q e) = showQuant q e
    showsPrec _ _            = error "instance Show Exp: unreachable case"

instance Show (Exp ExpTuple) where
    showsPrec _ (LitE l)     = shows l
    showsPrec _ (TupleE xs)  = showTuple $ map shows xs
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
instance Show ExpTuple where
    showsPrec _ (ExpTuple xs) = showTuple $ map shows xs

-- Show a quantified expression, e.g. (Forall x. (x + 1))
showQuant :: Show e => Quantifier -> e -> ShowS
showQuant q e = showSExpr [ showString (show q) . showChar '.'
                          , shows e
                          ]

showVar (Bound n) = showString ("#" ++ show n)
showVar (Free n)  = showString ("x" ++ show n)

-- Show a literal but be a litle bit more compact if the literal is redundant.
-- If the literal is the unit for the given CA operator, return Nothing.
-- Otherwise, return a ShowS.
showLiteral :: Show a => CAUOp a -> a -> Maybe ShowS
showLiteral op l
    | l `isUnitOf` op = Nothing
    | otherwise       = Just $ shows l

-------------------------------------------------------------------------------
-- Syntactic equality on expressions

-- Decide whether two expressions are equal, taking into account commutativity
-- and associativity of CA expressions, and alpha-renaming.
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

       (TupleE es1, TupleE es2) ->
          length es1 == length es2 && and (zipWith expEqual es1 es2)

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
-- Converting a tuple to a list of expressions

unzipExpTuple :: Exp ExpTuple -> [Exp Int]
unzipExpTuple (QuantE q e) = map (QuantE q) $ unzipExpTuple e
unzipExpTuple (TupleE es)  = es
unzipExpTuple _            = error "unzipExpTuple: Unexpected expression type"

-------------------------------------------------------------------------------
-- Simplification rules

-- This is the main rule for simplifying an expression.
--
-- First, subexpressions are simplified (simplifyRec).
-- Then "basic" simplifications are performed.  These restructure the
-- current term, but no other terms.
-- Then complex simplifications are performed that restructure the current
-- term and subtems.
--
-- To simplify, it's necessary to know the expression's free variables.
 
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
       TupleE es -> TupleE $ map simplify es
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

-- Look up a variable based on its de Bruijn index.
lookupVar :: Int -> [VarHandle] -> VarHandle
lookupVar 1 (v : vars) = v
lookupVar n (v : vars) | n > 0 = lookupVar (n - 1) vars
                       | otherwise = error "lookupVar: index must be greater \
                                           \than zero"
lookupVar _ []         = error "lookupVar: variable index out of range"

-- Convert a boolean expression to a formula.  The list of free variables is
-- passed as a parameter.
expToFormula :: [VarHandle] -> Exp Bool -> FormulaDef
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

       VarE _ -> error "expToFormula: boolean variables cannot be \
                       \converted to a formula"

       QuantE q e -> let body v = expToFormula (v:freeVars) e
                     in case q
                        of Forall -> qForall body
                           Exists -> qExists body

       _ -> error "expToFormula: unexpected expression"

sumToConstraint :: [VarHandle] -> Exp Int -> ([Coefficient], Int)
sumToConstraint freeVars expr =
    case deconstructSum expr
    of (constant, terms) -> (map deconstructTerm terms, constant)
    where
      deconstructTerm expr =
          case deconstructProduct expr
          of (n, [VarE (Bound i)]) -> Coeff (lookupVar i freeVars) n
             (_, [VarE (Free _)]) -> error "sumToConstraint: cannot use \
                                           \free variable in constraint"
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
      rn (TupleE es)      = TupleE $ map rn es
      rn (QuantE q e)     = QuantE q $ rename (bumpIndex v1) (bumpIndex v2) e

      -- Increment a de Bruijn index
      bumpIndex (Bound n)  = Bound (n+1)
      bumpIndex v@(Free _) = v

-- Convert some free variables to bound variables.  De Bruijn indices are
-- adjusted assuming the binder is added outside all existing binders.
--
-- The parameter is the list of free variables, starting with the outermost.
bindVariables :: [Int] -> Exp t -> Exp t
bindVariables fvs expr = bind expr
    where
      fvmap   = IntMap.fromList $ zip fvs [1..]
      fvLen   = length fvs
      shift n = n + fvLen

      bind :: forall t. Exp t -> Exp t
      bind (CAUE op lit es) = CAUE op lit $ map bind es
      bind (PredE op e)     = PredE op $ bind e
      bind (NotE e)         = NotE $ bind e
      bind expr@(LitE _)    = expr
      bind expr@(VarE v)    = case v
                              of Free n -> case IntMap.lookup n fvmap
                                           of Just n' -> VarE (Bound n')
                                              Nothing -> expr
                                 Bound n -> VarE $ Bound (shift n)
      bind (TupleE es)      = TupleE $ map bind es
      bind (QuantE q e)     = QuantE q $ bind e

-- Adjust bound variable bindings by adding 'shift' to all bound variable
-- indices greater than or equal to 'offset'.
adjustBindings :: Int -> Int -> Exp t -> Exp t
adjustBindings offset shift e = adj e
    where
      adj :: Exp t -> Exp t
      adj (CAUE op lit es) = CAUE op lit $ map adj es
      adj (PredE op e)     = PredE op $ adj e
      adj (NotE e)         = NotE $ adj e
      adj expr@(LitE _)    = expr
      adj expr@(VarE v)    = case v
                             of Free n -> expr
                                Bound n | n >= offset
                                            -> VarE $ Bound (n + shift)
                                        | otherwise
                                            -> expr
      adj (TupleE es)      = TupleE $ map adj es
      adj (QuantE q e)     = QuantE q $ adjustBindings (offset + 1) shift e

