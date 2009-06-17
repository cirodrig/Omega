
module Data.Omega.Parse
    (readBoolExp, readSet, readRelation, ParseError)
where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec hiding(digit)
import Text.ParserCombinators.Parsec.Expr

import Data.Omega.Expr
import Data.Omega.Set
import Data.Omega.Rel

type FreeVarID = Int
type BoundVarID = Int

type ExpParser a = GenParser Char ExpParserState a

-- The parser state keeps track of variable names and
-- the identifier of the innermost variable.
data ExpParserState =
    EPS { -- Free variables
          freeVars  :: Map String FreeVarID
        , -- Bound variables
          boundVars :: [Maybe String]
          -- List of available free variable IDs.
        , free      :: [FreeVarID]
        }

initialParserState :: ExpParserState
initialParserState = EPS
    { freeVars  = Map.empty
    , boundVars = []
    , free      = [1..]
    }

-- Look up a variable name.  Fail if name is not found.
lookupName :: String -> ExpParser Var
lookupName name = do
  s <- getState
  lookupBoundVariable s
    where
      -- First look in bound variables.  These are allowed to shadow free ones.
      lookupBoundVariable s =
          case findIndex nameMatches $ boundVars s of
            Just n -> return (Bound $ n+1) -- findIndex counts from zero;
                                           -- increment because we count from
                                           -- one
            Nothing -> lookupFreeVariable s

      lookupFreeVariable s =
          case Map.lookup name $ freeVars s of
            Just v  -> return (Free v)
            Nothing -> invalidName

      invalidName = fail $ "Undefined variable '" ++ name ++ "'"

      nameMatches (Just str) = name == str
      nameMatches Nothing    = False

-- Add a bound variable variable name.
-- Names are allowed to shadow previously bound names.
bindName :: String -> ExpParser ()
bindName name = updateState $ \st ->
    st { boundVars = Just name:boundVars st }

-- Bind an anonymous variable.
bindAnonymous :: ExpParser ()
bindAnonymous = updateState $ \st ->
    st { boundVars = Nothing:boundVars st }

bindAnonymouses :: Int -> ExpParser ()
bindAnonymouses n = sequence_ $ replicate n bindAnonymous

-- Undo the innermost variable.
unbind :: ExpParser ()
unbind = updateState $ \st ->
    st { boundVars = tail $ boundVars st }

-------------------------------------------------------------------------------
-- Primitive parsers

after :: ExpParser a -> ExpParser b -> ExpParser b
p `after` q = do x <- q
                 p
                 return x

identifierChar :: ExpParser Char
identifierChar = satisfy isAlpha

identifierOrNumber :: ExpParser Char
identifierOrNumber = satisfy isAlphaNum

digit :: ExpParser Int
digit = liftM digitToInt $ satisfy isDigit

lexeme :: ExpParser a -> ExpParser a
lexeme p = many space `after` p

delimiters :: ExpParser a -> ExpParser a -> ExpParser b -> ExpParser b
delimiters left right p = do left
                             right `after` p

identifier :: ExpParser String
identifier =
    lexeme $ try $ do name <- readName
                      if name `notElem` reservedWords
                        then return name
                        else fail $ "'" ++ name ++ "' is a reserved word"
    where
      readName = liftM2 (:) identifierChar (many identifierOrNumber)

integer :: ExpParser Int
integer = lexeme $ try $ optionalMinus $ digit >>= digits
    where
      digits n = (digits . ((10 * n) +) =<< digit) <|> return n
      optionalMinus p = (char '-' >> liftM negate p) <|> p

boolean :: ExpParser Bool
boolean = choice [ lexeme (try (string "true")) >> return True
                 , lexeme (try (string "false")) >> return False
                 ]

operator :: String -> ExpParser ()
operator name =
    lexeme $ try $ string name >> notFollowedBy (satisfy isPunctuation)

dot :: ExpParser ()
dot = lexeme (char '.') >> return ()

parens, braces, brackets :: ExpParser a -> ExpParser a
parens = delimiters (lexeme $ char '(') (lexeme $ char ')')
braces = delimiters (lexeme $ char '{') (lexeme $ char '}')
brackets = delimiters (lexeme $ char '[') (lexeme $ char ']')

reservedWords = ["forall", "exists"]

-- An infix operator parser
infixop s mk assoc = Infix (operator s >> return mk) assoc

-- A commutative and associative operation
ca :: CAUOp t -> Exp t -> Exp t -> Exp t
ca op = \x y -> CAUE op (unit op) [x, y]

-- Produces "x - y"
diff x y = CAUE Sum 0 [x, CAUE Prod (-1) [y]]

-- Parse a tuple in list syntax
tuple :: ExpParser a -> ExpParser [a]
tuple p = brackets $ p `sepBy` lexeme (char ',')

-------------------------------------------------------------------------------
-- Parsers

-- Parse a reference to a variable
var :: ExpParser Var
var = lookupName =<< identifier

-- Parse a binding of a variable.  The caller should unbind the variable
-- when it goes out of scope.
varBinding :: ExpParser ()
varBinding = bindName =<< identifier

-- Parse a variable binding, introduced by 'forall' or 'exists'.
binder :: ExpParser (Exp t) -> ExpParser (Exp t)
binder p = forallBinder <|> existsBinder
    where
      forallBinder = do
        lexeme $ try $ string "forall"
        body Forall

      existsBinder = do
        lexeme $ try $ string "exists"
        body Exists

      body q = do varBinding
                  dot
                  unbind `after` liftM (QuantE q) p

intTerm :: ExpParser (Exp Int)
intTerm = liftM VarE var
      <|> liftM LitE integer
      <|> parens intExp

intExp :: ExpParser (Exp Int)
intExp = buildExpressionParser intOperators intTerm
    where
      intOperators =
          [ [infixop "*" (ca Prod) AssocLeft]
          , [infixop "+" (ca Sum) AssocLeft, infixop "-" diff AssocNone]
          ]

data RelationToken = EqTok | GETok | LETok | GTTok | LTTok

-- An equality or inequality constraint.
constraint :: ExpParser (Exp Bool)
constraint = try $ do e1 <- intExp
                      tok <- relationToken
                      e2 <- intExp
                      return $ mkRel tok e1 e2
    where
      relationToken = lexeme $ choice [ operator "=" >> return EqTok
                                      , operator ">=" >> return GETok
                                      , operator "<=" >> return LETok
                                      , operator ">" >> return GTTok
                                      , operator "<" >> return LTTok
                                      ]

      mkRel EqTok e1 e2 = PredE IsZero $ diff e1 e2
      mkRel GETok e1 e2 = PredE IsGEZ $ diff e1 e2
      mkRel LETok e1 e2 = PredE IsGEZ $ diff e2 e1
      mkRel GTTok e1 e2 = PredE IsGEZ $ CAUE Sum (-1) [e1, CAUE Prod (-1) [e2]]
      mkRel LTTok e1 e2 = PredE IsGEZ $ CAUE Sum (-1) [e2, CAUE Prod (-1) [e1]]

boolTerm :: ExpParser (Exp Bool)
boolTerm = liftM LitE boolean
       <|> constraint
       <|> binder boolExp
       <|> parens boolExp

boolExp :: ExpParser (Exp Bool)
boolExp = buildExpressionParser boolOperators boolTerm
    where
      boolOperators =
          [ [Prefix (operator "!" >> return NotE)]
          , [infixop "&&" (ca Conj) AssocLeft]
          , [infixop "||" (ca Disj) AssocLeft]
          ]

set :: ExpParser Set
set = braces $ do
  -- The set starts with a tuple that
  -- binds some forall'd variables
  numVars <- liftM length $ tuple varBinding

  -- Read an optional "|" followed by a boolean formula 
  f <- option (LitE True) $ do try $ lexeme (char '|')
                               liftM simplify $ boolExp

  -- Unbind the variables
  sequence_ $ replicate numVars unbind

  return $ Set numVars f

relation :: ExpParser Rel
relation = braces $ do
  -- The set starts with a tuple that
  -- binds some forall'd variables
  numVars <- liftM length $ tuple varBinding

  -- Then an arrow
  lexeme (string "->")

  -- Then another tuple of expressions, written in terms of the
  -- input variables
  outExps <- tuple intExp

  -- Read an optional "|" followed by a boolean formula 
  f <- option (LitE True) $ do try $ lexeme (char '|')
                               liftM simplify $ boolExp

  -- Unbind the variables
  sequence_ $ replicate numVars unbind

  -- Return the relation
  return $ Rel numVars (TupleE outExps) f

readBoolExp :: String -> Either ParseError (Exp Bool)
readBoolExp input =
    runParser (eof `after` boolExp) initialParserState "<input>" input

readSet :: String -> Either ParseError Set
readSet input =
    runParser (eof `after` set) initialParserState "<input>" input

readRelation :: String -> Either ParseError Rel
readRelation input =
    runParser (eof `after` relation) initialParserState "<input>" input
