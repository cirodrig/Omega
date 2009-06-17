
{-# OPTIONS_GHC -XForeignFunctionInterface -fwarn-incomplete-patterns
                -XEmptyDataDecls -XRankNTypes -XMultiParamTypeClasses
                -XFunctionalDependencies -XTypeSynonymInstances
                -XFlexibleInstances -XFlexibleContexts #-}

module Data.Presburger.Omega.LowLevel
    (Presburger,
     OmegaSet,
     OmegaRel,
     isLowerBoundSatisfiable, isUpperBoundSatisfiable,
     isObviousTautology, isDefiniteTautology,
     isExact, isInexact, isUnknown, queryDNFSet,

     VarHandle,
     Coefficient(..),
     FormulaDef,
     conjunction, disjunction, negation,
     qForall, qExists,
     inequality, equality,
     true, false,
     newOmegaSet, newOmegaRel
     )
where

#include "C_omega.h"

#let alignof x = "%d", __alignof__(x)

import Control.Monad
import Data.Int
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import qualified Foreign.Marshal.Alloc as ForeignAlloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)

-- External data types, these have the same name as in C.
data Relation                   -- A set or relation
data Formula                    -- A logic formula
data F_And                      -- A conjunction
data F_Declaration              -- A forall or exists formula
data Var_Decl                   -- A handle to a variable
data DNF_Iterator               -- Iterator over a DNF clause
data Conjunct                   -- One conjunct within a DNF clause
data Tuple_Iterator a           -- Iterator over a Tuple (in Omega library)
data EQ_Iterator                -- Iterator over a set of EQ constraints
data EQ_Handle                  -- Handle to an EQ constraint
data GEQ_Iterator               -- Iterator over a set of GEQ constraints
data GEQ_Handle                 -- Handle to a GEQ constraint
data Constr_Vars_Iter           -- Iterate over coefficients in a constraint

class Constraint a

instance Constraint EQ_Handle
instance Constraint GEQ_Handle

-- Pointers to external data types
type C_Relation         = Ptr Relation
type C_Formula          = Ptr Formula
type C_And              = Ptr F_And
type C_Quantifier       = Ptr F_Declaration
type C_Var              = Ptr Var_Decl
type C_DNF_Iterator     = Ptr DNF_Iterator
type C_Conjunct         = Ptr Conjunct
type C_Tuple_Iterator a = Ptr (Tuple_Iterator a)
type C_EQ_Iterator      = Ptr EQ_Iterator
type C_EQ_Handle        = Ptr EQ_Handle
type C_GEQ_Iterator     = Ptr GEQ_Iterator
type C_GEQ_Handle       = Ptr GEQ_Handle
type C_Constr_Vars_Iter = Ptr Constr_Vars_Iter

-- Everything containing a formula is an instance of class Logical
class Logical f where
    add_and    :: f -> IO C_Formula
    add_or     :: f -> IO C_Formula
    add_not    :: f -> IO C_Formula
    add_forall :: f -> IO C_Quantifier
    add_exists :: f -> IO C_Quantifier
    convert_to_and :: f -> IO C_And
    finalize   :: f -> IO ()

instance Logical C_Relation where
    add_and    = relation_add_and
    add_or     = relation_add_or
    add_not    = relation_add_not
    add_forall = relation_add_forall
    add_exists = relation_add_exists
    -- We take advantage of the fact that C_And is a subclass of C_Formula
    -- here, and simply cast the pointer.
    convert_to_and r = liftM castPtr $ relation_add_and r
    finalize   = relation_finalize

instance Logical C_Formula where
    add_and    = formula_add_and
    add_or     = formula_add_or
    add_not    = formula_add_not
    add_forall = formula_add_forall
    add_exists = formula_add_exists
    convert_to_and = formula_to_and
    finalize   = formula_finalize

-- C_And is a subclass of C_Formula and implements all its methods.
-- Consequently, we simply cast to C_Formula
instance Logical C_And where
    add_and    = formula_add_and . castPtr
    add_or     = formula_add_or . castPtr
    add_not    = formula_add_not . castPtr
    add_forall = formula_add_forall . castPtr
    add_exists = formula_add_exists . castPtr
    convert_to_and = return
    finalize   = formula_finalize . castPtr

-- C_Quantifier is a subclass of C_Formula and implements all its methods.
-- Consequently, we simply cast to C_Formula
instance Logical C_Quantifier where
    add_and    = formula_add_and . castPtr
    add_or     = formula_add_or . castPtr
    add_not    = formula_add_not . castPtr
    add_forall = formula_add_forall . castPtr
    add_exists = formula_add_exists . castPtr
    convert_to_and = formula_to_and . castPtr
    finalize   = formula_finalize . castPtr

-- Used for freeing data that was allocated in C
foreign import ccall safe free :: Ptr a -> IO ()

foreign import ccall safe "HS_omega.h" new_relation
    :: CInt -> CInt -> IO C_Relation
foreign import ccall safe "HS_omega.h" new_set
    :: CInt -> IO C_Relation
foreign import ccall safe "HS_omega.h" free_relation
    :: C_Relation -> IO ()
foreign import ccall "HS_omega.h &free_relation" ptr_to_free_relation
    :: FunPtr (C_Relation -> IO ())
foreign import ccall safe "HS_omega.h" relation_show
    :: C_Relation -> IO CString
foreign import ccall safe "HS_omega.h" input_var
    :: C_Relation -> CInt -> IO C_Var
foreign import ccall safe "HS_omega.h" output_var
    :: C_Relation -> CInt -> IO C_Var
foreign import ccall safe "HS_omega.h" set_var
    :: C_Relation -> CInt -> IO C_Var
foreign import ccall safe "HS_omega.h" is_lower_bound_satisfiable
    :: C_Relation -> IO Bool
foreign import ccall safe "HS_omega.h" is_upper_bound_satisfiable
    :: C_Relation -> IO Bool
foreign import ccall safe "HS_omega.h" is_obvious_tautology
    :: C_Relation -> IO Bool
foreign import ccall safe "HS_omega.h" is_definite_tautology
    :: C_Relation -> IO Bool
foreign import ccall safe "HS_omega.h" is_exact
    :: C_Relation -> IO Bool
foreign import ccall safe "HS_omega.h" is_inexact
    :: C_Relation -> IO Bool
foreign import ccall safe "HS_omega.h" is_unknown
    :: C_Relation -> IO Bool

foreign import ccall safe "HS_omega.h" relation_add_and
    :: C_Relation -> IO C_Formula
foreign import ccall safe "HS_omega.h" relation_add_or
    :: C_Relation -> IO C_Formula
foreign import ccall safe "HS_omega.h" relation_add_not
    :: C_Relation -> IO C_Formula
foreign import ccall safe "HS_omega.h" relation_add_forall
    :: C_Relation -> IO C_Quantifier
foreign import ccall safe "HS_omega.h" relation_add_exists
    :: C_Relation -> IO C_Quantifier
foreign import ccall safe "HS_omega.h" relation_finalize
    :: C_Relation -> IO ()

-- These functions take formula pointer arguments
foreign import ccall safe "HS_omega.h" formula_add_and
    :: C_Formula -> IO C_Formula
foreign import ccall safe "HS_omega.h" formula_add_or
    :: C_Formula -> IO C_Formula
foreign import ccall safe "HS_omega.h" formula_add_not
    :: C_Formula -> IO C_Formula
foreign import ccall safe "HS_omega.h" formula_add_forall
    :: C_Formula -> IO C_Quantifier
foreign import ccall safe "HS_omega.h" formula_add_exists
    :: C_Formula -> IO C_Quantifier
foreign import ccall safe "HS_omega.h" formula_finalize
    :: C_Formula -> IO ()

foreign import ccall safe "HS_omega.h" declaration_declare
    :: C_Quantifier -> IO C_Var

-- If the argument is a C_And, the argument is returned;
-- otherwise, add_and is called
foreign import ccall safe "HS_omega.h" formula_to_and
    :: C_Formula -> IO C_And

foreign import ccall safe "HS_omega.h" add_constraint
    :: C_And -> Bool -> CInt -> Ptr CInt -> Ptr C_Var -> CInt -> IO ()

-- Look at the internal representation of a set
foreign import ccall safe "HS_omega.h" query_dnf
    :: C_Relation -> IO C_DNF_Iterator
foreign import ccall safe "HS_omega.h" dnf_iterator_next
    :: C_DNF_Iterator -> IO C_Conjunct
foreign import ccall safe "HS_omega.h" dnf_iterator_free
    :: C_DNF_Iterator -> IO ()

-- For inspecting Omega data structures
foreign import ccall safe "HS_omega.h" get_conjunct_variables
    :: C_Conjunct -> IO (C_Tuple_Iterator C_Var)
foreign import ccall safe "HS_omega.h" tuple_iterator_next
    :: (C_Tuple_Iterator (Ptr a)) -> IO (Ptr a)
foreign import ccall safe "HS_omega.h" tuple_iterator_free
    :: (C_Tuple_Iterator a) -> IO ()

foreign import ccall safe "HS_omega.h" get_eqs
    :: C_Conjunct -> IO C_EQ_Iterator
foreign import ccall safe "HS_omega.h" eqs_next
    :: C_EQ_Iterator -> IO C_EQ_Handle
foreign import ccall safe "HS_omega.h" eqs_free
    :: C_EQ_Iterator -> IO ()
foreign import ccall safe "HS_omega.h" eq_handle_free
    :: C_EQ_Handle -> IO ()

foreign import ccall safe "HS_omega.h" get_geqs
    :: C_Conjunct -> IO C_GEQ_Iterator
foreign import ccall safe "HS_omega.h" geqs_next
    :: C_GEQ_Iterator -> IO C_GEQ_Handle
foreign import ccall safe "HS_omega.h" geqs_free
    :: C_GEQ_Iterator -> IO ()
foreign import ccall safe "HS_omega.h" geq_handle_free
    :: C_GEQ_Handle -> IO ()

foreign import ccall safe "HS_omega.h" constraint_get_const
    :: Ptr a -> IO #{type coefficient_t}
foreign import ccall safe "HS_omega.h" constraint_get_coefficients
    :: Ptr a -> IO C_Constr_Vars_Iter
foreign import ccall safe "HS_omega.h" constr_vars_next
    :: Ptr Coefficient -> C_Constr_Vars_Iter -> IO Bool 
foreign import ccall safe "HS_omega.h" constr_vars_free
    :: C_Constr_Vars_Iter -> IO ()


-- For debugging
foreign import ccall safe "HS_omega.h" debug_print_eq
    :: C_EQ_Handle -> IO ()
foreign import ccall safe "HS_omega.h" debug_print_geq
    :: C_GEQ_Handle -> IO ()

-------------------------------------------------------------------------------
-- Exported interface

data OmegaSet = OmegaSet { sPtr :: {-# UNPACK #-} !(ForeignPtr Relation)
                         , sDom :: [VarHandle]
                         }

data OmegaRel = OmegaRel { rPtr :: {-# UNPACK #-} !(ForeignPtr Relation)
                         , rDom :: [VarHandle]
                         , rRng :: [VarHandle]
                         }

-- Omega sets and relations are in class Presburger
class Presburger a where
    pPtr :: a -> ForeignPtr Relation

instance Presburger OmegaSet where
    pPtr = sPtr

instance Presburger OmegaRel where
    pPtr = rPtr

-- Wrap a relation or set as a ForeignPtr so it gets finalized
wrapOmegaSet :: C_Relation -> [VarHandle] -> IO OmegaSet
wrapOmegaSet ptr dom = do
  foreignptr <- newForeignPtr ptr_to_free_relation ptr
  return $! OmegaSet { sPtr = foreignptr
                     , sDom = dom
                     }

wrapOmegaRel :: C_Relation -> [VarHandle] -> [VarHandle] -> IO OmegaRel
wrapOmegaRel ptr dom rng = do
  foreignptr <- newForeignPtr ptr_to_free_relation ptr
  return $! OmegaRel { rPtr = foreignptr
                     , rDom = dom
                     , rRng = rng }

-- Use a wrapped relation or set
withPresburger :: Presburger a => a -> (C_Relation -> IO b) -> IO b
withPresburger p = withForeignPtr (pPtr p)

instance Show OmegaSet where
    show rel = unsafePerformIO $ withPresburger rel $ \ptr -> do
        -- Call relation_show to get a C string, then convert to String
        cStr <- relation_show ptr
        str  <- peekCString cStr
        free cStr
        return str

instance Show OmegaRel where
    show rel = unsafePerformIO $ withPresburger rel $ \ptr -> do
        -- Call relation_show to get a C string, then convert to String
        cStr <- relation_show ptr
        str  <- peekCString cStr
        free cStr
        return str

isLowerBoundSatisfiable, isUpperBoundSatisfiable, isObviousTautology,
    isDefiniteTautology, isExact, isInexact, isUnknown
    :: Presburger a => a -> IO Bool

isLowerBoundSatisfiable rel = withPresburger rel is_lower_bound_satisfiable
isUpperBoundSatisfiable rel = withPresburger rel is_upper_bound_satisfiable
isObviousTautology rel      = withPresburger rel is_obvious_tautology
isDefiniteTautology rel     = withPresburger rel is_definite_tautology
isExact rel                 = withPresburger rel is_exact
isInexact rel               = withPresburger rel is_inexact
isUnknown rel               = withPresburger rel is_unknown

-- A handle to a variable
newtype VarHandle = VarHandle { unVarHandle :: C_Var }

-- A formula definition
newtype FormulaDef = FD {runFD :: forall a. Logical a => a -> IO ()}
 
-- A multiplicative coefficient for a variable.
data Coefficient = Coeff { coeffVar :: {-# UNPACK #-} !VarHandle
                         , coeffValue :: {-# UNPACK #-} !Int}

instance Show Coefficient where
    show (Coeff v n) = "(" ++ show n ++ " * " ++ show (unVarHandle v) ++ ")"
instance Storable Coefficient where
    sizeOf _ = #{size Variable_Info_struct}
    alignment _ = #{alignof Variable_Info_struct}
    peek p = do
      var  <- #{peek Variable_Info_struct, var} p :: IO C_Var
      coef <- #{peek Variable_Info_struct, coef} p :: IO #{type coefficient_t}
      return $ Coeff { coeffVar = VarHandle var
                     , coeffValue = fromIntegral coef
                     }

-- Create an omega relation.
-- The omega library lets you initialize an Omega relation,
-- then start putting formulas inside of it.  This
-- interface takes an initializer function as a parameter.  
newOmegaSet :: Int -> ([VarHandle] -> FormulaDef) -> IO OmegaSet
newOmegaSet numVars init = do
  rel <- new_set (fromIntegral numVars)

  -- Look up the ID for each variable in the tuple.  Variables are ordered
  -- from last to first because the last variable is "innermost," has
  -- de Bruijn index 1, and belongs at position 1 in the list.
  freeVarIDs <- mapM (liftM VarHandle . set_var rel)
                [fromIntegral numVars, fromIntegral numVars - 1 .. 1]

  runFD (init freeVarIDs) rel
  wrapOmegaSet rel freeVarIDs

newOmegaRel :: Int
            -> Int
            -> ([VarHandle] -> [VarHandle] -> FormulaDef)
            -> IO OmegaRel
newOmegaRel numInputs numOutputs init = do
  rel <- new_relation (fromIntegral numInputs) (fromIntegral numOutputs)

  -- Look up the IDs for the input and output variables.
  outputVarIds <- mapM (liftM VarHandle . output_var rel)
                  [fromIntegral numOutputs, fromIntegral numOutputs - 1 .. 1]
  inputVarIds <- mapM (liftM VarHandle . input_var rel)
                 [fromIntegral numInputs, fromIntegral numInputs - 1 .. 1]

  runFD (init inputVarIds outputVarIds) rel
  wrapOmegaRel rel inputVarIds outputVarIds

conjunction, disjunction :: [FormulaDef] -> FormulaDef
conjunction formulaDefs = FD $ \f -> do
  newF <- add_and f
  mapM_ (\func -> runFD func newF) formulaDefs
  finalize newF

disjunction formulaDefs = FD $ \f -> do
  newF <- add_or f
  mapM_ (\func -> runFD func newF) formulaDefs
  finalize newF

negation :: FormulaDef -> FormulaDef
negation formulaDef = FD $ \f -> do
  newF <- add_not f
  runFD formulaDef newF
  finalize newF

qForall, qExists :: (VarHandle -> FormulaDef) -> FormulaDef
qForall makeBody = FD $ \f -> do
  newFormula <- add_forall f
  localVar <- declaration_declare newFormula
  runFD (makeBody (VarHandle localVar)) newFormula
  finalize newFormula

qExists makeBody = FD $ \f -> do
  newFormula <- add_exists f
  localVar <- declaration_declare newFormula
  runFD (makeBody (VarHandle localVar)) newFormula
  finalize newFormula

addConstraint :: Bool -> [Coefficient] -> Int -> C_And -> IO ()
addConstraint kind terms constant formula = do
  let numTerms     = length terms
      numTermsCInt = fromIntegral numTerms
      constantCInt = fromIntegral constant
      coefficients = map (fromIntegral . coeffValue) terms
      variables    = map ((\(VarHandle h) -> h) . coeffVar) terms

  -- Marshal the coefficients and variables to C as arrays
  withArray coefficients $ \coeffPtr ->
      withArray variables $ \varPtr ->
          -- then, call code to set the constraint
          add_constraint formula kind numTermsCInt coeffPtr varPtr constantCInt

inequality :: [Coefficient] -> Int -> FormulaDef
inequality terms constant = FD $ \formula ->
    addConstraint False terms constant =<< convert_to_and formula

equality :: [Coefficient] -> Int -> FormulaDef
equality terms constant = FD $ \formula ->
    addConstraint True terms constant =<< convert_to_and formula

true :: FormulaDef
true = equality [] 0

false :: FormulaDef
false = equality [] 1

-------------------------------------------------------------------------------
-- Marshalling from Omega Library to Haskell

class Iterator i a | i -> a where next :: i -> IO a

instance Iterator C_DNF_Iterator C_Conjunct where
    next = dnf_iterator_next

instance Iterator (C_Tuple_Iterator (Ptr a)) (Ptr a) where
    next = tuple_iterator_next

instance Iterator C_EQ_Iterator C_EQ_Handle where
    next = eqs_next

instance Iterator C_GEQ_Iterator C_GEQ_Handle where
    next = geqs_next

foreach :: Iterator i (Ptr b) => (a -> (Ptr b) -> IO a) -> a -> i -> IO a
foreach f x iter = visit x
    where
      visit x = do y <- next iter
                   if y == nullPtr
                     then return x
                     else visit =<< f x y

-- Iterate through each conjunct in a DNF clause
iterateDNF :: (a -> C_Conjunct -> IO a) -> a -> C_Relation -> IO a
iterateDNF f x rel = do
  iter <- query_dnf rel
  x' <- foreach f x iter
  dnf_iterator_free iter
  return x'

iterateConjVars :: (a -> C_Var -> IO a) -> a -> C_Conjunct -> IO a
iterateConjVars f x conj = do
  iter <- get_conjunct_variables conj
  x' <- foreach f x iter
  tuple_iterator_free iter
  return x'

iterateEqs :: (a -> C_EQ_Handle -> IO a) -> a -> C_Conjunct -> IO a
iterateEqs f x conj = do
  iter <- get_eqs conj
  x' <- foreach wrapped_f x iter
  eqs_free iter
  return x'
    where
      -- This wrapper just makes sure the handle is freed after use
      wrapped_f x eqHdl = do 
        x' <- f x eqHdl
        eq_handle_free eqHdl
        return x'

iterateGeqs :: (a -> C_GEQ_Handle -> IO a) -> a -> C_Conjunct -> IO a
iterateGeqs f x conj = do
  iter <- get_geqs conj
  x' <- foreach wrapped_f x iter
  geqs_free iter
  return x'
    where
      -- This wrapper just makes sure the handle is freed after use
      wrapped_f x geqHdl = do 
        x' <- f x geqHdl
        geq_handle_free geqHdl
        return x'

peekConstraintVars :: Constraint a => Ptr a -> IO [Coefficient]
peekConstraintVars cst = do
  iter <- constraint_get_coefficients cst

  -- Allocate temporary storage on the C side for some data
  c_var_info <- ForeignAlloc.malloc

  -- Traverse and pull out values
  coeffs <- getCoefficients iter c_var_info []

  -- Free the temporary storage and the iterator
  ForeignAlloc.free c_var_info
  constr_vars_free iter

  return coeffs
    where
      getCoefficients iter c_var_info coeffs = do

        -- Read one coefficient
        ok <- constr_vars_next c_var_info iter

        -- If it returned false, we're done
        if not ok then return coeffs else do
        coeff <- peek c_var_info
        getCoefficients iter c_var_info (coeff:coeffs)

queryDNFSet :: OmegaSet -> IO ()
queryDNFSet s = withPresburger s $ iterateDNF foo ()
    where
      foo acc conjunct = do
        -- Find existentially bound variables in this conjunct, which
        -- Omega calls "wildcard variables"
        wildcardVars <- iterateConjVars findWildcards [] conjunct

        -- For each EQ relation, get the relation
        iterateEqs (const test_eq) () conjunct

        -- For each GE relation, get the relation
        iterateGeqs (const test_eq) () conjunct

        return ()

      findWildcards acc var =
          -- Is this an input variable?
          case findIndex (var ==) (map unVarHandle $ sDom s)
          of Just n  -> return acc
             Nothing -> -- Otherwise, assume it's a wildcard
                        -- FIXME: call into C to check the variable's kind
                        return $ var : acc

      test_eq :: Constraint a => Ptr a -> IO ()
      test_eq eq = do
        -- debug_print_eq eq
        coefficients <- peekConstraintVars eq
        constant <- constraint_get_const eq

        print coefficients
        print constant
        return ()
