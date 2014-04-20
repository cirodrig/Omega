
{-# OPTIONS_GHC -XBangPatterns
                -XTypeFamilies
                -XEmptyDataDecls
                -XFlexibleInstances
                -XFlexibleContexts
                -XUndecidableInstances #-}
module Data.Presburger.Omega.Internal.ShowExpr where

import Data.List
import Data.Presburger.Omega.Internal.Expr

instance Show (Exp Int) where
    showsPrec n e =
        showsIntExprPrec emptyShowsEnv n (getSimplifiedExpr e)

instance Show (Exp Bool) where
    showsPrec n e =
        showsBoolExprPrec emptyShowsEnv n (getSimplifiedExpr e)

-------------------------------------------------------------------------------
-- Showing expressions

appPrec = 10
mulPrec = 7
addPrec = 6
cmpPrec = 4                     -- Less-than, equal
andPrec = 3
lamPrec = 0                     -- Body of lambda

-- An environment for showing expressions.
--
-- Quantified variables are shown as lambda-bound variables.  This structure
-- keeps track of lambda-bound variable names and how to show them.

data ShowsEnv =
    ShowsEnv
    { -- How to show the n_th bound variable, given a precedence context
      showNthVar :: ![Int -> ShowS]
      -- Number of bound variables we know about.
      --   numBound e == length (showNthVar e)
    , numBound   :: !Int
      -- Names for new bound variables
    , varNames   :: [ShowS]
    }

emptyShowsEnv =
    ShowsEnv
    { showNthVar = []
    , numBound = 0
    , varNames = map showChar $
                 ['x', 'y', 'z'] ++
                 ['a' .. 'w'] ++
                 [error "out of variable names"]
    }

--  Show a ShowsEnv for debugging
showShowsEnv :: ShowsEnv -> String
showShowsEnv env =
  concat ["  " ++ show lamPrec "\n" | show <- showNthVar env]

-- Add a variable binding to the environment
bindVariable :: ShowsEnv -> (ShowS, ShowsEnv)
bindVariable env =
    case varNames env
    of nm : nms ->
           let env' = ShowsEnv
                      { showNthVar = showVar nm : showNthVar env
                      , numBound   = 1 + numBound env
                      , varNames   = nms
                      }
           in (nm, env')
    where
      -- Showing a variable produces "varE varName"
      showVar nm n = showParen (n >= appPrec) $ showString "varE " . nm

bindVariables :: Int -> ShowsEnv -> ([ShowS], ShowsEnv)
bindVariables 0 env = ([], env)
bindVariables n env = let (v, env') = bindVariable env
                          (vs, env'') = bindVariables (n-1) env'
                      in (vs ++ [v], env'')

showsVarPrec :: ShowsEnv -> Int -> Var -> ShowS
showsVarPrec env prec (Bound i) =
    if i < numBound env
    then (showNthVar env !! i) prec
    else shift (numBound env)
    where
      -- The variable is not bound locally, so show its constructor.
      -- We have to subtract an offset to account for the local variable
      -- bindings, basically undoing the shift that 'withFreshVariable'
      -- applies.
      shift n = showParen (prec >= appPrec) $
                    showString "nthVarE " . shows (i-n)

-- Unique is not showable, but users shouldn't see quantified variables anyway
showsVarPrec _ _ (Quantified u) = showString "(Quantified _)"

showsInt :: Int -> ShowS
showsInt n | n >= 0    = showString "intE " . shows n
           | otherwise = showString "intE " . showParen True (shows n)


showsIntExprPrec :: ShowsEnv -> Int -> IntExpr -> ShowS
showsIntExprPrec env n expression =
    case expression
    of CAUE Sum lit es  -> showParen (n >= addPrec) $ showSum env lit es
       CAUE Prod lit es -> showParen (n >= mulPrec) $ showProd env lit es
       LitE l           -> showParen (n >= appPrec) $
                           showsInt l
       VarE v           -> showsVarPrec env n v

showsBoolExprPrec :: ShowsEnv -> Int -> BoolExpr -> ShowS
showsBoolExprPrec env n expression =
    case expression
    of CAUE Conj lit es
           | lit        -> let texts = map (showsBoolExprPrec env 0) es
                           in showParen (n >= fromIntegral andPrec) $
                              texts `showSepBy` showString " |&&| "
           | otherwise  -> showString "falseE"
       CAUE Disj lit es
           | lit        -> showString "trueE"
           | otherwise  -> let texts = map (showsBoolExprPrec env 0) es
                           in showParen (n >= appPrec) $
                              showString "disjE " . showsList texts
       PredE IsGEZ e    -> showGEZ env n e
       PredE IsZero e   -> showEQZ env n e
       NotE e           -> showString "notE " . showsBoolExprPrec env appPrec e
       LitE True        -> showString "trueE"
       LitE False       -> showString "falseE"
       QuantE q e       -> showParen (n >= appPrec) $
                           showQuantifier showsBoolExprPrec env q e

-- Show an inequality prettily.
-- First, eliminate minus-signs.
-- Then, choose between the ">", ">=", or "<" for displaying a term.
--
-- If one side of the inequality is a literal, it 
-- Use ">" if it gets rid of a term, otherwise use ">=".
-- If the left side of the inequality is an integer literal,
-- then move it to the right 
showGEZ :: ShowsEnv -> Int -> IntExpr -> ShowS
showGEZ env prec e =
    showParen (prec >= cmpPrec) $
    case e
    of LitE n          -> showGEZ' env n []
       CAUE Sum lit es -> showGEZ' env lit es
       _               -> showGEZ' env 0 [e]

showGEZ' env lit es =
    -- Partition into terms that will go on the left (positive) and right
    -- (negative) sides of the inequality.  Try to get rid of a '1' by
    -- using a greater-than sign.
    case partitionSumBySign lit es
    of (-1, neg, pos) -> balanceInequality False 0 neg pos
       (n, neg, pos)  -> balanceInequality True n neg pos
    where
      -- If the left side is empty, flip the direction of the inequality
      balanceInequality True n neg [] =
          showInequality le (negate n) [] neg

      balanceInequality False n neg [] =
          showInequality lt (negate n) [] neg

      balanceInequality True n neg pos =
          showInequality ge n neg pos
          
      balanceInequality False n neg pos =
          showInequality gt n neg pos

      -- Show the inequality.  Put the literal on whichever side makes it
      -- positive.
      showInequality symbol lit neg pos =
          let (pos', neg') =
                  if lit >= 0
                  then (CAUE Sum lit pos, CAUE Sum 0 neg)
                  else (CAUE Sum 0 pos, CAUE Sum (negate lit) neg)
          in showsIntExprPrec env cmpPrec pos' .
             symbol .
             showsIntExprPrec env cmpPrec neg'

      ge = showString " |>=| "
      gt = showString " |>| "
      le = showString " |<=| "
      lt = showString " |<| "


-- Show a comparison-to-zero prettily.
-- If it is a summation with only one term, then we show it as
--   "term |==| literal"
-- Otherwise we show it as an application of "isZeroE".
showEQZ :: ShowsEnv -> Int -> IntExpr -> ShowS
showEQZ env prec (CAUE Sum lit [e]) =
    showParen (prec >= cmpPrec) $
    (showsIntExprPrec env cmpPrec e .
     showString " |==| " .
     showsIntExprPrec env cmpPrec (LitE lit)
    )

showEQZ env prec e =
    showParen (prec >= appPrec) $
    showString "isZeroE " . showsIntExprPrec env appPrec e

-- Partition a sum term based on the sign it is displayed with.
-- Negative-signed terms are multiplied by -1 to make them positive.
partitionSumBySign n es =
    case partition hasNegativeMultiplier es
    of (neg, pos) -> let neg' = map negateMultiplier neg
                     in (n, neg', pos)
    where
      hasNegativeMultiplier :: IntExpr -> Bool
      hasNegativeMultiplier (CAUE Prod n es) = n < 0
      hasNegativeMultiplier (LitE n) = n < 0
      hasNegativeMultiplier _ = False

      negateMultiplier :: IntExpr -> IntExpr
      negateMultiplier (CAUE Prod n es) = CAUE Prod (negate n) es
      negateMultiplier (LitE n) = LitE (negate n)
      negateMultiplier _ = error "partitionSumBySign: unexpected term"

-- Show a sum term
showSum env lit es =
    -- The first element of the summation gets shown a little differently.
    -- There are a couple of cases, depending on what is the first element.
    if lit == 0
    then case es
         of e : es' -> showsIntExprPrec env addPrec e . showSumTail es'
            []      -> showsInt 0
    else showsInt lit . showSumTail es
    where
      -- Show the tail of a sum term.  Each expression is preceded by
      -- the |+| or |-| operator.
      showSumTail es = foldr (.) id $ map showSumTailElement es

      showSumTailElement e =
          case deconstructProduct e
          of Term 1 es             -> add . showProd env 1 es
             Term (-1) es          -> sub . showProd env 1 es
             Term n es | n >= 0    -> add . showProd env n es
                       | otherwise -> sub . showProd env (negate n) es

      add = showString " |+| "
      sub = showString " |-| "

-- Show a product term
showProd env lit es =
    let text = map (showsIntExprPrec env mulPrec) es
        textLit = if lit == 1
                  then id
                  else showsPrec mulPrec lit . showString " *| "
    in textLit . (text `showSepBy` showString " |*| ")

-- Show a list in [,,] syntax
showsList :: [ShowS] -> ShowS
showsList ss =
    showChar '[' . (ss `showSepBy` showString ", ") . showChar ']'

-- Show a list with a separator interspersed
showSepBy :: [ShowS] -> ShowS -> ShowS
xs `showSepBy` sep = foldr (.) id (intersperse sep xs)

-- Show a quantified expression, e.g. (forallE $ \x -> varE x |+| intE 1)
showQuantifier :: (ShowsEnv -> Int -> Expr t -> ShowS)
               -> ShowsEnv -> Quantifier -> Expr t -> ShowS
showQuantifier showExpr env q e =
    let quantifier = case q
                     of Forall -> showString "forallE $ \\"
                        Exists -> showString "existsE $ \\"

        -- Take a new variable name
        (varName, env') = bindVariable env

    in quantifier . varName . showString " -> " . showExpr env' lamPrec e

-- Show a term with a lambda-bound parameter
showLambdaBound :: (ShowsEnv -> Int -> ShowS)
                -> ShowsEnv -> Int -> ShowS
showLambdaBound showBody env prec =
  let -- Take a new variable name
      (varName, env') = bindVariable env

  in showParen (prec >= appPrec) $
     showChar '\\' . varName . showString " -> " .
     showBody env' lamPrec

-- Show a term with a lambda-bound list parameter
showLambdaList :: Int
               -> (ShowsEnv -> Int -> ShowS)
               -> ShowsEnv -> Int -> ShowS
showLambdaList n_params showBody env prec =
  let -- Take a new variable name
      (varNames, env') = bindVariables n_params env

  in (showString ("[[" ++ showShowsEnv env' ++ "]]") .) $  -- DEBUG
     showParen (prec >= appPrec) $
     showChar '\\' . showsList varNames . showString " -> " .
     showBody env' lamPrec

