
{-# OPTIONS_GHC -XBangPatterns
                -XTypeFamilies
                -XEmptyDataDecls
                -XFlexibleInstances
                -XFlexibleContexts
                -XUndecidableInstances #-}
module Data.Presburger.Omega.Internal.ShowExpr
       (showIntExpr, showBoolExpr, showLambdaBound)
where

import Data.List
import Data.Presburger.Omega.Internal.Expr
import Data.Presburger.Omega.Internal.ShowUtil

instance Show (Exp Int) where
  showsPrec n e = showsPrecExpr (showIntExpr $ getSimplifiedExpr e) n

instance Show (Exp Bool) where
  showsPrec n e = showsPrecExpr (showBoolExpr $ getSimplifiedExpr e) n

-------------------------------------------------------------------------------
-- Integer expressions

showIntExpr :: IntExpr -> ShowExpr 
showIntExpr (LitE l)         = showIntLit l
showIntExpr (VarE v)         = showVar v
showIntExpr (CAUE Sum l es)  = showSum l es
showIntExpr (CAUE Prod l es) = showProd l es

showIntLit :: Int -> ShowExpr
showIntLit n = showTerminal "intE" `showApp` showInt n

showSum :: Int -> [IntExpr] -> ShowExpr
showSum lit es =
  let (s, ss) = sumTerms lit es
  in foldl addTerm s ss
  where
    addTerm s (SumTerm is_add e) = showLeftfixOp addPrec op s e
      where 
        op = if is_add then "|+|" else "|-|"

-- | Temporary representation for one term in a summation.  The 'Bool'
--   is true if the term is shown with a \"+\", false if shown with a \"-\".
data SumTerm = SumTerm !Bool ShowExpr

-- | Split a summation into showable terms
sumTerms :: Int -> [IntExpr] -> (ShowExpr, [SumTerm])
sumTerms lit []   = (showIntLit lit, [])
sumTerms 0 (e:es) = (showIntExpr e, map showSumTerm es)
sumTerms n es     = (showIntLit n, map showSumTerm es)

-- Show a term of a summation.  Avoid showing multiplication where possible.
showSumTerm e =
  let Term n es = deconstructProduct e
  in SumTerm (n >= 0) (showProd (abs n) es)

-- Show a product term.  Use the "*|" operator with an initial literal.
showProd n [] = showIntLit n
showProd n (e:es) =
  let first = if n == 1
              then showIntExpr e
              else showLeftfixOp mulPrec "*|" (showInt n) (showIntExpr e)
  in showLeftfixCAOp mulPrec "|*|" (Just first) $ map showIntExpr es

-------------------------------------------------------------------------------
-- Boolean expressions

showBoolExpr :: BoolExpr -> ShowExpr
showBoolExpr (CAUE Conj lit []) = showBoolLit lit
showBoolExpr (CAUE Conj lit es) =
  let b = if lit then Nothing else Just $ showBoolLit lit
  in showLeftfixCAOp andPrec "|&&|" b $ map showBoolExpr es

showBoolExpr (CAUE Disj lit es) =
  let ss = map showBoolExpr es
      terms = if lit then showBoolLit lit : ss else ss
  in showTerminal "disjE" `showApp` showAsList ss

showBoolExpr (PredE IsGEZ e)  = showGEZ e
showBoolExpr (PredE IsZero e) = showEQZ e

showBoolExpr (NotE e) = showTerminal "notE" `showApp` showBoolExpr e

showBoolExpr (LitE l) = showBoolLit l

showBoolExpr (QuantE q e) = showQuantified q e

showBoolLit True = showTerminal "trueE"
showBoolLit False = showTerminal "falseE"

-- Show an inequality prettily.
-- First, eliminate minus-signs.
-- Then, choose between the ">", ">=", or "<" for displaying a term.
--
-- Use ">" if it gets rid of a term, otherwise use ">=".
-- If the left side of the inequality is an integer literal,
-- then move it to the right 
showGEZ (LitE n)        = showGEZ' n []
showGEZ (CAUE Sum n es) = showGEZ' n es
showGEZ e               = showGEZ' 0 [e]

showGEZ' lit es =
    -- Partition into terms that will go on the left (positive) and right
    -- (negative) sides of the inequality.  Try to get rid of a '1' by
    -- using a greater-than sign.
    case partitionSumBySign lit es
    of (-1, neg, pos) -> balanceInequality False 0 neg pos
       (n, neg, pos)  -> balanceInequality True n neg pos
    where
      -- If the left side is empty, flip the direction of the inequality
      balanceInequality True n neg [] =
          showInequality "|<=|" (negate n) [] neg

      balanceInequality False n neg [] =
          showInequality "|<|" (negate n) [] neg

      balanceInequality True n neg pos =
          showInequality "|>=|" n neg pos
          
      balanceInequality False n neg pos =
          showInequality "|>|" n neg pos

      -- Show the inequality.  Put the literal on whichever side makes it
      -- positive.
      showInequality symbol lit neg pos =
          let (pos', neg') =
                  if lit >= 0
                  then (CAUE Sum lit pos, CAUE Sum 0 neg)
                  else (CAUE Sum 0 pos, CAUE Sum (negate lit) neg)
          in showLeftfixOp cmpPrec symbol 
             (showIntExpr pos')
             (showIntExpr neg')

-- Show a comparison-to-zero prettily.
-- If it is a summation with only one term, then we show it as
--   "term |==| literal"
-- Otherwise we show it as an application of "isZeroE".
showEQZ (CAUE Sum lit [e]) =
  showLeftfixOp cmpPrec "|==|" (showIntExpr e) (showIntLit lit)

showEQZ e =
  showTerminal "isZeroE" `showApp` showIntExpr e

-- Partition a sum term based on the sign it is displayed with.
-- Negative-signed terms are multiplied by -1 to make them positive.
partitionSumBySign n es =
  let (pos, neg) = partition fst $ map extractSign es
  in (n, map snd neg, map snd pos)
    where
      -- Extract the sign from a term.
      extractSign :: IntExpr -> (Bool, IntExpr)
      extractSign (CAUE Prod n es) = (n >= 0, CAUE Prod (abs n) es)
      extractSign (LitE n)         = (n >= 0, LitE $ abs n)
      extractSign e                = (True, e)

-- Show a quantified expression such as 
-- @forallE (\x -> varE x |==| intE 1)@
showQuantified q e =
  let quantifier = case q
                   of Forall -> showTerminal "forallE"
                      Exists -> showTerminal "existsE"
  in quantifier `showApp` showLambda bindVariable (\v -> (v, showBoolExpr e))
  
-- Show a lambda expression taking a parameter list such as 
-- @\[x, y] -> varE x |+| varE y |==| intE 1@
showLambdaBound n s =
  showLambda (bindVariables n) (\vs -> (showAsList vs, s))
  
