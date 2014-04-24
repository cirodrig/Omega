
{-# OPTIONS_GHC -XBangPatterns #-}
module Data.Presburger.Omega.Internal.ShowUtil where

import Data.List

import Data.Presburger.Omega.Internal.Expr

-------------------------------------------------------------------------------
-- Showing with precedence

appPrec, mulPrec, addPrec, cmpPrec, andPrec, lamPrec :: Int

appPrec = 10
mulPrec = 7
addPrec = 6
cmpPrec = 4                     -- Less-than, equal
andPrec = 3
lamPrec = 0                     -- Body of lambda

-- | An expression with optional parentheses, depending on the precedence of  
--   its context.
newtype ShowPrec = ShowPrec (Int -> ShowS)

infix 8 @=, @<

-- | Show an expression in the given precedence context
(@=) :: ShowPrec -> Int -> ShowS
ShowPrec f @= x = f x

-- | Show an expression in a precedence context weaker than the given context
(@<) :: ShowPrec -> Int -> ShowS
ShowPrec f @< x = f (pred x)

-- | Rename (.) at different precedence
(+++) = (.)
infixr 2 +++

-- | Parenthesize if precedence is greater than local_prec
withPrec :: Int -> ShowS -> ShowPrec
withPrec local_prec s = ShowPrec (\ctx -> showParen (ctx >= local_prec) s)

fromShowS :: ShowS -> ShowPrec
fromShowS s = ShowPrec (const s)

-------------------------------------------------------------------------------

data ShowsEnv =
    ShowsEnv
    { -- How to show the n_th bound variable, given a precedence context
      showNthVar :: ![ShowPrec]
      -- Number of bound variables we know about.
      --   numBound e == length (showNthVar e)
    , numBound   :: !Int
      -- Names for new bound variables
    , varNames   :: [Char]
    }

emptyShowsEnv =
    ShowsEnv
    { showNthVar = []
    , numBound = 0
    , varNames = ['x', 'y', 'z'] ++
                 ['a' .. 'w'] ++
                 [error "out of variable names"]
    }

-- Add a variable binding to the environment
extendShowsEnv :: ShowsEnv -> ShowsEnv
extendShowsEnv env =
  let !(nm : nms) = varNames env

      -- Showing a variable produces the variable name
      showVar = fromShowS $ showChar nm
  in ShowsEnv { showNthVar = showVar : showNthVar env
              , numBound   = 1 + numBound env
              , varNames   = nms
              }

showVarPrec :: ShowsEnv -> Var -> ShowPrec
showVarPrec env (Bound i) =
    if i < numBound env
    then showNthVar env !! i
    else shift (numBound env)
    where
      -- The variable is not bound locally, so show its constructor.
      -- We have to subtract an offset to account for the local variable
      -- bindings, basically undoing the shift that 'withFreshVariable'
      -- applies.
      shift n = withPrec appPrec $ showString "nthVariable " . shows (i-n)

-- Unique is not showable, but users shouldn't see quantified variables anyway
showVarPrec _ (Quantified u) = withPrec appPrec $ showString "Quantified _"

-------------------------------------------------------------------------------

newtype ShowExpr = ShowExpr {showExpr :: ShowsEnv -> ShowPrec}

runShowExpr :: ShowExpr -> String
runShowExpr e = (showExpr e emptyShowsEnv @= lamPrec) ""

fromShowPrec :: ShowPrec -> ShowExpr
fromShowPrec s = ShowExpr (const s)

fromShowSAsExpr :: ShowS -> ShowExpr
fromShowSAsExpr = fromShowPrec . fromShowS

-- | For compatibility with 'showsPrec'
showsPrecExpr :: ShowExpr -> Int -> ShowS
showsPrecExpr e n = showExpr e emptyShowsEnv @= n

showTerminal :: String -> ShowExpr
showTerminal s = fromShowPrec $ fromShowS $ showString s

showInt :: Int -> ShowExpr
showInt n = fromShowPrec show_int
  where
    -- Parenthesize negative numbers
    show_int =
      if n >= 0
      then fromShowS $ shows n
      else withPrec addPrec $ shows n

showVar :: Var -> ShowExpr
showVar v = showTerminal "varE" `showApp`
            (ShowExpr (\env -> showVarPrec env v))

showApp :: ShowExpr -> ShowExpr -> ShowExpr
showApp f x = ShowExpr (\env -> withPrec appPrec $
                                showExpr f env @< appPrec +++
                                showChar ' ' +++
                                showExpr x env @= appPrec)

showLeftfixOp :: Int -> String -> ShowExpr -> ShowExpr -> ShowExpr
showLeftfixOp prec name l r =
  ShowExpr (\env -> withPrec prec $
                    showExpr l env @< prec +++
                    showChar ' ' +++
                    showString name +++
                    showChar ' ' +++
                    showExpr r env @= prec)

-- | Show a chain of values with infix operators, @x * x1 * ... * xN@.
--
--   There must be at least one value.
--   The optional value is a literal, which may be left out.
showLeftfixCAOp :: Int -> String -> Maybe ShowExpr -> [ShowExpr] -> ShowExpr
showLeftfixCAOp prec name mx xs = 
  case (mx, xs)
  of (Just x,  xs)   -> slo x xs
     (Nothing, x:xs) -> slo x xs
     -- Other cases not permitted
  where
    slo x xs = foldl (showLeftfixOp prec name) x xs

showAsList :: [ShowExpr] -> ShowExpr
showAsList xs = ShowExpr $ \env ->
  fromShowS $
  showChar '[' +++
  foldr (+++) id (intersperse (showString ", ") [showExpr x env @= lamPrec | x <- xs]) +++
  showChar ']'

showLambda :: ((a -> ShowExpr) -> ShowExpr)
           -> (a -> (ShowExpr, ShowExpr))
           -> ShowExpr
showLambda binder shower = binder build
  where
    build param =
      let !(bind_s, body_s) = shower param
      in ShowExpr $ \env ->
           withPrec lamPrec $
           showChar '\\' +++
           showExpr bind_s env @= appPrec +++
           showString " -> " +++
           showExpr body_s env @= lamPrec

-- | Bind a local variable
bindVariable :: (ShowExpr -> ShowExpr) -> ShowExpr
bindVariable f = ShowExpr $ \env ->
  let env' = extendShowsEnv env
      new_var = fromShowPrec $ head $ showNthVar env'
  in showExpr (f new_var) env'

-- | Bind a list of variables
bindVariables :: Int -> ([ShowExpr] -> ShowExpr) -> ShowExpr
bindVariables 0 f = f []

bindVariables n f =
  bindVariable $ \v -> bindVariables (n-1) $ \vs -> f (v : vs)
