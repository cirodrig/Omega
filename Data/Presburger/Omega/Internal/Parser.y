{
module Data.Presburger.Omega.Internal.Parser
  (parseIntExp, parseBoolExp, parseSet, parseRel)
where

import Control.Applicative
import qualified Data.Map as Map
import Data.Presburger.Omega.Internal.Expr
import Data.Presburger.Omega.Set(Set, setFromExp)
import Data.Presburger.Omega.Rel(Rel, relFromExp)
import Data.Presburger.Omega.Internal.Lexer

}

%name parseIntExp  Arith
%name parseBoolExp Logic
%name parseSet     Set
%name parseRel     Rel

%tokentype { Token }
%error { parseError }

%token
        '('                     { LParenTok }
        ')'                     { RParenTok }
        '['                     { LBrackTok }
        ']'                     { RBrackTok }
        '\\'                    { LambdaTok }
        ','                     { CommaTok }
        '->'                    { ToTok }
        nthVariable             { Tok_nthVariable }
        intE                    { Tok_intE }
        varE                    { Tok_varE }
        notE                    { Tok_notE }
        disjE                   { Tok_disjE }
        forallE                 { Tok_forallE }
        existsE                 { Tok_existsE }
        trueE                   { Tok_trueE }
        falseE                  { Tok_falseE }
        isZeroE                 { Tok_isZeroE }
        set                     { Tok_set }
        rel                     { Tok_rel }
        '*|'                    { IntTimesTok }
        '|*|'                   { TimesTok }
        '|+|'                   { PlusTok }
        '|-|'                   { MinusTok }
        '|&&|'                  { AndTok }
        '|<=|'                  { LETok }
        '|<|'                   { LTTok }
        '|>=|'                  { GETok }
        '|>|'                   { GTTok }
        '|==|'                  { EQTok }
        int                     { IntTok $$ }
        ident                   { IdentTok $$ }

%left '|&&|'
%left '|+|' '|-|'
%left '*|' '|*|'

%%

--  A variable reference
Var     : ident                         { lookupName $1 }
        | '(' nthVariable int ')'       { freeVar $3 }

--  A variable name that defines a new variable
Bind    : ident                         { $1 }

Cmp     :: { IntExp -> IntExp -> BoolExp }
        : '|>|'                         { (|>|) }
        | '|>=|'                        { (|>=|) }
        | '|<|'                         { (|<|) }
        | '|<=|'                        { (|<=|) }
        | '|==|'                        { (|==|) }

Paren(op)
        : '(' op ')'                    { $2 }

OptParen(op)
        : op                            { $1 }
        | Paren(op)                     { $1 }

List(op)
        : '[' SepBy(',',op) ']'         { $2 }

SepBy(sep,op)
        :                               { pure [] }
        | op StartBy(sep,op)            { liftA2 (:) $1 $2 }

StartBy(sep,op)
        :                               { pure [] }
        | sep op StartBy(sep,op)        { liftA2 (:) $2 $3 }

ListPure(op)
        : '[' SepByPure(',',op) ']'     { $2 }

SepByPure(sep,op)
        :                               { [] }
        | op StartByPure(sep,op)        { $1 : $2 }

StartByPure(sep,op)
        :                               { pure [] }
        | sep op StartByPure(sep,op)    { $2 : $3 }

-- An integer term
IntTerm : intE OptParen(int)            { pure (intE $2) }
        | varE Var                      { liftA varE $2 }
        | Paren(Arith)                  { $1 }

-- An arithmetic term with infix operators
Arith   :: { P IntExp }
        : IntTerm                       { $1 }
        | OptParen(int) '*|' Arith      { liftA ($1 *|) $3 }
        | Arith '|*|' IntTerm           { liftA2 (|*|) $1 $3 }
        | Arith '|+|' Arith             { liftA2 (|+|) $1 $3 }
        | Arith '|-|' Arith             { liftA2 (|-|) $1 $3 }

QuantBody(p)
        : '\\' Bind '->' p              { bindName $2 $4 }

Quant(p)
        : Paren(QuantBody(p))           { $1 }

-- A boolean term
BoolTerm :: { P BoolExp }
        : disjE List(Logic)             { liftA disjE $2 }
        | forallE Quant(Logic)          { liftA rawForall $2 }
        | existsE Quant(Logic)          { liftA rawExists $2 }
        | isZeroE IntTerm               { liftA isZeroE $2 }
        | notE BoolTerm                 { liftA notE $2 }
        | trueE                         { pure trueE }
        | falseE                        { pure falseE }
        | Paren(Logic)                  { $1 }

-- | A logical term with infix operators
Logic   :: { P BoolExp }
        : Arith Cmp Arith               { liftA2 $2 $1 $3 }
        | BoolTerm                      { $1 }
        | Logic '|&&|' Logic            { liftA2 (|&&|) $1 $3 }

ListQuantBody(p)
        : '\\' ListPure(Bind) '->' p    { bindNames $2 $4 }

ListQuant(p)
        : Paren(ListQuantBody(p))       { $1 }

-- The set dimension is nonnegative, so it will never be in parentheses
SetBody :: { P Set }
        : set int ListQuant(Logic)      { liftA (setFromExp $2) $3 }

Set     : SetBody                       { $1 }
        | Paren(SetBody)                { $1 }

-- The input and ouput dimensions are nonnegative,
-- so they will never be in parentheses
RelBody :: { P Rel }
        : rel int int ListQuant(ListQuant(Logic)) { liftA (relFromExp $2 $3) $4 }

Rel     : RelBody                       { $1 }
        | Paren(RelBody)                { $1 }

{

-- An environment mapping bound variable names to their de Bruijn indices
data Env = Env {numBound :: !Int, boundVars :: Map.Map String Int}

emptyEnv = Env 0 Map.empty

-- Insert a new variable into the context.
-- Shift the indices of all bound variables.
insertVar name (Env n m) =
  Env (n+1) (Map.insert name 0 $ Map.map (1+) m)

insertVars names (Env n m) =
  let n_names = length names
      shifted = Map.map (n_names +) m
      extended = foldr (uncurry Map.insert) shifted (zip names [0..])
  in Env (n + n_names) extended

-- Parsing uses an environment of bound variable names
newtype P a = P (Env -> a)

runP (P f) = f emptyEnv

instance Functor P where fmap f (P g) = P (f . g)

instance Applicative P where
  pure x = P (const x)
  P f <*> P g = P (\e -> f e (g e))

lookupName :: String -> P Var
lookupName name = P $ \env ->
  case Map.lookup name $ boundVars env
  of Just n  -> nthVariable n
     Nothing -> error $ "Undefined variable: " ++ name

-- Create a free variable.  Shift the variable ID by the number of bound
-- variables.
freeVar :: Int -> P Var
freeVar n = P $ \env ->
  nthVariable (n + numBound env)

bindName :: String -> P a -> P a
bindName name (P parse_fn) = P (parse_fn . insertVar name)

bindNames :: [String] -> P a -> P a
bindNames names (P parse_fn) = P (parse_fn . insertVars names)

rawForall e = wrapExpr $ QuantE Forall $ getExpr e
rawExists e = wrapExpr $ QuantE Exists $ getExpr e

parseError ts = error $ "Parse error:" ++ show ts

parseIntExp :: [Token] -> P IntExp

instance Read IntExp where
  readsPrec _ s = [(runP (parseIntExp $ alexScanTokens s), "")]

instance Read BoolExp where
  readsPrec _ s = [(runP (parseBoolExp $ alexScanTokens s), "")]

instance Read Set where
  readsPrec _ s = [(runP (parseSet $ alexScanTokens s), "")]

instance Read Rel where
  readsPrec _ s = [(runP (parseRel $ alexScanTokens s), "")]

}
