
module Pretty
    (pprIntExp, pprBoolExp, toDoc)
where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc)
import Expr

-- Names for variables
type Names = [Doc]

freshNames :: Names
freshNames = map PP.char $
             ['a'..'z'] ++ [error "Too many bound variables to print"]

-- Pretty-printing maintains a list of bound variable names and a
-- list of fresh variable names
data PprNames = PprNames { bound, free :: Names }

initialNames = PprNames [] freshNames

-- Bind another variable
pushName :: PprNames -> PprNames
pushName (PprNames {bound = b, free = (new:f)}) =
    PprNames {bound = (new:b), free = f}

lookupName :: Var -> PprNames -> Doc
lookupName (Bound n) names = bound names !! (n-1)

-------------------------------------------------------------------------------
-- Pretty-printing combinators

data Precedence =
    Outermost | DisjP | ConjP | NotP | PredicateP | AddP | MulP | Innermost
    deriving (Eq, Ord, Enum)

newtype Ppr = Ppr { pr :: Precedence -> PprNames -> Doc }

toDoc :: Ppr -> Doc
toDoc (Ppr f) = f Outermost initialNames

ppr :: Precedence -> Doc -> Ppr
ppr prec d = Ppr $ \_ _ -> d

parens :: Precedence -> Ppr -> Ppr
parens prec x = Ppr $ \ctx names -> if ctx >= prec
                                    then PP.parens (pr x prec names)
                                    else pr x prec names

liftPpr :: (Doc -> Doc) -> (Ppr -> Ppr)
liftPpr f (Ppr x) = Ppr $ \p names -> f (x p names)

liftPpr2 :: (Doc -> Doc -> Doc) -> (Ppr -> Ppr -> Ppr)
liftPpr2 f (Ppr x) (Ppr y) = Ppr $ \p names -> f (x p names) (y p names)

liftPprs :: ([Doc] -> Doc) -> ([Ppr] -> Ppr)
liftPprs f xs = Ppr $ \p names -> f $ map (\x -> pr x p names) xs

at :: Ppr -> Precedence -> Ppr
x `at` p = Ppr $ \_ names -> pr x p names

text s = ppr Innermost (PP.text s)
char c = ppr Innermost (PP.char c)
fsep = liftPprs (PP.fsep) 
(<+>) = liftPpr2 (PP.<+>)
(<>) = liftPpr2 (PP.<>)
hang x n y = Ppr $ \prec names -> PP.hang (pr x prec names) n (pr y prec names)

-------------------------------------------------------------------------------

pprIntExp :: Exp Int -> Ppr
pprIntExp expr =
    case expr
    of CAUE op lit es -> pprCA op lit es
       LitE l         -> literal l
       VarE v         -> pprVar v
       QuantE q e     -> quantifier q e
    where
      literal l = text $ show l
      quantifier q e = pprQuantifier pprIntExp q e

pprBoolExp :: Exp Bool -> Ppr
pprBoolExp expr =
    case expr
    of CAUE op lit es -> pprCA op lit es
       PredE IsZero e -> predicate "=" e
       PredE IsGEZ  e -> predicate ">=" e
       NotE e         -> char '!' <> pprBoolExp e
       LitE True      -> text "true"
       LitE False     -> text "false"
       VarE v         -> pprVar v
       QuantE q e     -> quantifier q e
    where
      quantifier q e = pprQuantifier pprBoolExp q e

-- Docstring formatting for a predicate test
predicate operator e = parens PredicateP $
    pprIntExp e <+> text operator <+> char '0'

-- A "paragraph fill" bunch of terms with operators interspersed
ca operator prec terms =
    let pprs = map (<+> text operator) (init terms) ++ [last terms]
    in parens prec $ fsep $ pprs `at` prec

pprCA :: forall t. CAUOp t -> t -> [Exp t] -> Ppr
pprCA op lit es =
    let pprOperands prettyPrint =
            if lit `isUnitOf` op
            then map prettyPrint es
            else map prettyPrint (LitE lit : es)
    in case op
       of Sum  -> if lit == 1
                  then pprSum es
                  else pprSum (LitE lit : es)
          Prod -> ca "*" MulP $ pprOperands pprIntExp
          Conj -> ca "&&" ConjP $ pprOperands pprBoolExp
          Disj -> ca "||" DisjP $ pprOperands pprBoolExp

-- Create a product term that may or may not be negated.
-- If a Left value is returned, then the term should be negated, but it
-- may simply be subtracted in the surrounding context, instead.
-- If a Right value is returned, then the term is positive.
productSign :: Int -> [Exp Int] -> Either (Bool -> Ppr) Ppr
productSign lit es =
    case lit
    of 1             -> Right (product es)
       -1            -> Left (productNeg1 es)
       _ | lit < 0   -> Left (productNeg lit es)
         | otherwise -> Right (product (LitE lit : es))
    where
      productNeg1 es False = product (LitE (-1) : es)
      productNeg1 es True  = product es
      productNeg lit es False = product (LitE lit : es)
      productNeg lit es True  = product (LitE (negate lit) : es)
      product es = ca "*" MulP $ map pprIntExp es

pprProd lit es names = case productSign lit es
                       of Left f  -> f False
                          Right x -> x

pprProductContext :: Exp Int -> Either (Bool -> Ppr) Ppr
pprProductContext (CAUE Prod lit es) = productSign lit es
pprProductContext e = Right $ pprIntExp e

pprSum es = fsep $ doSum $ map pprProductContext es
    where
      -- Make the first docstring
      doSum (Left  f : es) = doSum' (f False) es
      doSum (Right d : es) = doSum' d         es

      -- Make the other docstrings.  One incomplete docstring is kept around
      -- each time.  After inspecting the next term in the sum, a '+' or '-'
      -- is appended to the preceding docstring.
      doSum' doc (Left f : es)  = (doc <+> char '-') : doSum' (f True) es
      doSum' doc (Right d : es) = (doc <+> char '+') : doSum' d        es
      doSum' doc []             = [doc]

pprQuantifier prettyPrint q e = Ppr $ \prec names ->
    let binder = quantifier q PP.<+> head (free names) PP.<> PP.char '.'
        body   = pr (prettyPrint e) Outermost $ pushName names
    in PP.hang binder 2 body
    where
      quantifier Forall = PP.text "forall" 
      quantifier Exists = PP.text "exists"

pprVar v = Ppr $ \_ names -> lookupName v names