
module Data.Presburger.Omega.Internal.Arbitrary where

import Control.Applicative
import Control.Monad
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Presburger.Omega.Internal.Expr

-- | Generate an arbitrary expression in @n@ free variables.
--   The expression is linear in the variables.
arbitraryLinearExpr :: Int -> Gen IntExpr
arbitraryLinearExpr n = do 
  coeff <- arbitrary
  coeffs <- replicateM n arbitrary
  return $ CAUE Sum coeff [CAUE Prod c [VarE $ nthVariable i]
                          | (i, c) <- zip [0..] coeffs]

-- | Generate an arbitrary Boolean expression in @n@ free variables.
--   Each integer subterm is linear in the variables.
arbitraryLinearBoolExpr :: Int -> Gen BoolExpr
arbitraryLinearBoolExpr nfv =
  frequency [(8, predicate),
             (3, conjunction),
             (3, disjunction),
             (3, negation),
             (3, quantified),
             (1, return $ LitE True),
             (1, return $ LitE False)]
  where
    reduce_size g = sized $ \n -> resize (n - (n+2) `div` 3) g
    conjunction = reduce_size $
                  conjExpr <$> listOf (arbitraryLinearBoolExpr nfv)
    disjunction = reduce_size $
                  disjExpr <$> listOf (arbitraryLinearBoolExpr nfv)
    negation    = NotE <$> arbitraryLinearBoolExpr nfv
    predicate   = PredE <$>
                    elements [IsZero, IsGEZ] <*>
                    arbitraryLinearExpr nfv
    quantified  = reduce_size $ QuantE <$>
                    elements [Forall, Exists] <*>
                    arbitraryLinearBoolExpr (nfv+1)
