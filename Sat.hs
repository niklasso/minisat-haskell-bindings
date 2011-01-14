{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Rank2Types #-}
module Sat
  -- solvers and literals
  ( Solver    -- :: (* -> *) -> * -> *
  , Lit       -- :: * -> *; Eq, Ord, Show
  , neg       -- :: Lit s -> Lit s
  
  -- a general interface
  , MonadSat(..) {-
      withNewSolver :: (forall s . Solver m s -> m a) -> m a
      newLit        :: Solver m s -> m (Lit s)
      addClause     :: Solver m s -> [Lit s] -> m Bool
      simplify      :: Solver m s -> m ()
      solve         :: Solver m s -> [Lit s] -> m Bool
      value         :: Solver m s -> Lit s -> m (Maybe Bool)
      modelValue    :: Solver m s -> Lit s -> m (Maybe Bool)
      conflict      :: Solver m s -> m [Lit s]
    -}

  -- for IO computations
  , SolverIO  -- SolverIO s = Solver IO s
  
  -- for pure functions
  , Sat       -- :: * -> * -> *; Monad, Functor
  , SolverSat -- SolverSat t s = Solver (Sat t) s
  , run       -- :: (forall t . Sat t a) -> a
  )
 where

import Foreign.Ptr
  ( Ptr
  , nullPtr
  )

import Foreign.C.Types
  ( CInt
  )

import System.IO.Unsafe
  ( unsafePerformIO
  )

import Control.Exception

----------------------------------------------------------------------------
-- class SatMonad

class Monad m => MonadSat m where
  withNewSolver :: (forall s . Solver m s -> m a) -> m a
  newLit        :: Solver m s -> m (Lit s)
  addClause     :: Solver m s -> [Lit s] -> m Bool
  simplify      :: Solver m s -> m ()
  solve         :: Solver m s -> [Lit s] -> m Bool
  value         :: Solver m s -> Lit s -> m (Maybe Bool)
  modelValue    :: Solver m s -> Lit s -> m (Maybe Bool)
  conflict      :: Solver m s -> m [Lit s]

----------------------------------------------------------------------------
-- instance for IO

type SolverIO s = Solver IO s

instance MonadSat IO where
  withNewSolver h =
    do s <- solver_new
       solver_set_verbosity s 0
       h s `finally` solver_delete s

  newLit s =
    do solver_newLit s

  addClause s xs =
    do solver_addClause_begin s
       sequence_ [ solver_addClause_addLit s x | x <- xs ]
       solver_addClause_commit s

  simplify s =
    do solver_simplify s

  solve s xs =
    do solver_solve_begin s
       sequence_ [ solver_solve_addLit s x | x <- xs ]
       solver_solve_commit s

  value s x =
    mbool `fmap` solver_value_Lit s x

  modelValue s x =
    mbool `fmap` solver_modelValue_Lit s x

  conflict s =
    do n <- solver_conflict_len s
       sequence [ solver_conflict_nthLit s i | i <- [0..n-1] ]

----------------------------------------------------------------------------
-- instance for Sat (monad for pure functions)

type SolverSat t s = Solver (Sat t) s

newtype Sat t a =
  MkSat{ unMkSat :: IO a }

instance Functor (Sat t) where
  f `fmap` MkSat m = MkSat (f `fmap` m)

instance Monad (Sat t) where
  return x =
    MkSat (return x)
  
  MkSat m >>= h =
    MkSat (m >>= \x -> unMkSat (h x))

-- safe run function
run :: (forall t . Sat t a) -> a
run m = unsafePerformIO (unMkSat m)

instance MonadSat (Sat t) where
  withNewSolver h =
    MkSat (withNewSolver (\s -> unMkSat (h (unsafeSolverCast s))))

  newLit s =
    MkSat (newLit (unsafeSolverCast s))

  addClause s xs =
    MkSat (addClause (unsafeSolverCast s) xs)

  simplify s =
    MkSat (simplify (unsafeSolverCast s))

  solve s xs =
    MkSat (solve (unsafeSolverCast s) xs)

  value s x =
    MkSat (value (unsafeSolverCast s) x)

  modelValue s x =
    MkSat (modelValue (unsafeSolverCast s) x)
    
  conflict s =
    MkSat (conflict (unsafeSolverCast s))

----------------------------------------------------------------------------
-- basic types

-- solver
newtype Solver t s = MkSolver (Ptr ())

unsafeSolverCast :: Solver t s -> Solver t' s
unsafeSolverCast (MkSolver p) = MkSolver p

-- var
newtype Var s = MkVar CInt
  deriving ( Eq, Ord )

instance Show (Var s) where
  show (MkVar n) = 'v' : show n

-- lit
newtype Lit s = MkLit CInt
  deriving ( Eq, Ord )

instance Show (Lit s) where
  show x = (if solver_sign x then "~" else "") ++ show (solver_var x) 

neg :: Lit s -> Lit s
neg = solver_negate

-- lbool
newtype LBool = MkLBool Int
  deriving (Eq, Ord)

instance Show LBool where
  show b
    | b == solver_get_l_False = "False"
    | b == solver_get_l_True  = "True"
    | otherwise               = "Undef"

mbool :: LBool -> Maybe Bool
mbool b 
  | b == solver_get_l_False = Just False
  | b == solver_get_l_True  = Just True
  | otherwise               = Nothing

----------------------------------------------------------------------------

foreign import ccall unsafe "static ../c/solver.h"
  solver_new :: IO (Solver t s)

foreign import ccall unsafe "static ../c/solver.h"
  solver_delete :: Solver t s -> IO ()

foreign import ccall unsafe "static ../c/solver.h" solver_newVar :: Solver t s -> IO (Var s)

foreign import ccall unsafe "static ../c/solver.h"
  solver_newLit :: Solver t s -> IO (Lit s)

foreign import ccall unsafe "static ../c/solver.h"
  solver_mkLit :: Var s -> Lit s

foreign import ccall unsafe "static ../c/solver.h"
  solver_mkLit_args :: Var s -> Bool -> Lit s

foreign import ccall unsafe "static ../c/solver.h"
  solver_negate :: Lit s -> Lit s

foreign import ccall unsafe "static ../c/solver.h"
  solver_var :: Lit s -> Var s

foreign import ccall unsafe "static ../c/solver.h"
  solver_sign :: Lit s -> Bool

foreign import ccall unsafe "static ../c/solver.h"
  solver_addClause :: Solver t s -> Int -> Ptr (Lit s) -> IO Bool
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_addClause_begin :: Solver t s -> IO ()
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_addClause_addLit :: Solver t s -> Lit s -> IO ()
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_addClause_commit :: Solver t s -> IO Bool
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_simplify :: Solver t s -> IO ()
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_solve :: Solver t s -> Int -> Ptr (Lit s) -> IO Bool
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_solve_begin :: Solver t s -> IO ()
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_solve_addLit :: Solver t s -> Lit s -> IO ()
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_solve_commit :: Solver t s -> IO Bool
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_okay :: Solver t s -> IO Bool
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_setPolarity :: Solver t s -> Var s -> Bool -> IO ()
 
foreign import ccall unsafe "static ../c/solver.h"
  solver_setDecisionVar :: Solver t s -> Var s -> Bool -> IO ()

foreign import ccall unsafe "static ../c/solver.h"
  solver_get_l_True :: LBool

foreign import ccall unsafe "static ../c/solver.h"
  solver_get_l_False :: LBool

foreign import ccall unsafe "static ../c/solver.h"
  solver_get_l_Undef :: LBool

foreign import ccall unsafe "static ../c/solver.h"
  solver_value_Var :: Solver t s -> Var s -> IO LBool

foreign import ccall unsafe "static ../c/solver.h"
  solver_value_Lit :: Solver t s -> Lit s -> IO LBool

foreign import ccall unsafe "static ../c/solver.h"
  solver_modelValue_Var :: Solver t s -> Var s -> IO LBool

foreign import ccall unsafe "static ../c/solver.h"
  solver_modelValue_Lit :: Solver t s -> Lit s -> IO LBool

-- orkar...
{-
int solver_num_assigns(solver *s);
int solver_num_clauses(solver *s);     
int solver_num_learnts(solver *s);     
int solver_num_vars(solver *s);  
int solver_num_freeVars(solver *s);
int solver_num_conflicts(solver *s);
-}

foreign import ccall unsafe "static ../c/solver.h"
  solver_conflict_len :: Solver t s -> IO Int
  
foreign import ccall unsafe "static ../c/solver.h"
  solver_conflict_nthLit :: Solver t s -> Int -> IO (Lit s)

foreign import ccall unsafe "static ../c/solver.h"
  solver_set_verbosity :: Solver t s -> Int -> IO ()

