{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MiniSat where

import Foreign.Ptr     ( Ptr, nullPtr )
import Foreign.C.Types ( CInt(..) )
import Control.Exception (bracket, finally, mask_, onException )
import Control.Concurrent.Async

#include "minisat.h"
#include "hsc-magic.h"

-- | Run a minisat instance in such a way that it is
-- interruptable (by sending killThread).
-- cf. https://github.com/niklasso/minisat-haskell-bindings/issues/1
withNewSolverAsync :: (Solver -> IO a) -> IO a
withNewSolverAsync h = 
  bracket newSolver deleteSolver $ \  s -> do
    mask_ $ withAsync (h s) $ \ a -> do
      wait a `onException` minisat_interrupt s

withNewSolver :: (Solver -> IO a) -> IO a
withNewSolver h =
  do s <- newSolver
     h s `finally` deleteSolver s

newSolver :: IO Solver
newSolver =
  do s <- minisat_new
     eliminate s True -- make the default behave as a normal solver (avoiding common bugs)
     return s

deleteSolver :: Solver -> IO ()
deleteSolver = minisat_delete

newLit :: Solver -> IO Lit
newLit = minisat_newLit

neg :: Lit -> Lit
neg = minisat_negate

addClause :: Solver -> [Lit] -> IO Bool
addClause s xs =
  do minisat_addClause_begin s
     sequence_ [ minisat_addClause_addLit s x | x <- xs ]
     minisat_addClause_commit s

simplify :: Solver -> IO Bool
simplify = minisat_simplify

eliminate :: Solver -> Bool -> IO Bool
eliminate = minisat_eliminate

setFrozen :: Solver -> Var -> Bool -> IO ()
setFrozen = minisat_setFrozen

isEliminated :: Solver -> Var -> IO Bool
isEliminated = minisat_isEliminated

{-
solve :: Solver -> [Lit] -> Model a -> IO (Either [Lit] a)
-}

solve :: Solver -> [Lit] -> IO Bool
solve s xs =
  do minisat_solve_begin s
     sequence_ [ minisat_solve_addLit s x | x <- xs ]
     minisat_solve_commit s

limited_solve :: Solver -> [Lit] -> IO LBool
limited_solve s xs =
  do minisat_solve_begin s
     sequence_ [ minisat_solve_addLit s x | x <- xs ]
     minisat_limited_solve_commit s

value, modelValue :: Solver -> Lit -> IO (Maybe Bool)
(value,modelValue) = (get minisat_value_Lit, get minisat_modelValue_Lit)
 where
  get f s x = mbool `fmap` f s x

  mbool b 
    | b == l_False = Just False
    | b == l_True  = Just True
    | otherwise    = Nothing

conflict :: Solver -> IO [Lit]
conflict s =
  do n <- minisat_conflict_len s
     sequence [ minisat_conflict_nthLit s i | i <- [0..n-1] ]

-- TODO: Is it possible to FFI C constants, instead of using a dummy function?
l_True, l_False, l_Undef :: LBool
l_True  = minisat_get_l_True
l_False = minisat_get_l_False
l_Undef = minisat_get_l_Undef

----------------------------------------------------------------------------

newtype Solver = MkSolver (Ptr ())
newtype Var    = MkVar CInt  deriving ( Eq, Ord )
newtype Lit    = MkLit CInt  deriving ( Eq, Ord )
newtype LBool  = MkLBool CInt deriving ( Eq, Ord )

instance Show Var where
  show (MkVar n) = 'v' : show n

instance Show Lit where
  show x = (if minisat_sign x then "~" else "") ++ show (minisat_var x) 

instance Show LBool where
  show b
    | b == l_False = "False"
    | b == l_True  = "True"
    | otherwise    = "Undef"

#define CTYPE_solver minisat_solver*
#define HTYPE_solver Solver
#define CTYPE_bool minisat_bool
#define HTYPE_bool Bool
#define CTYPE_lit minisat_Lit
#define HTYPE_lit Lit
#define CTYPE_int int
#define HTYPE_int Int
#define CTYPE_var minisat_Var
#define HTYPE_var Var
#define CTYPE_lbool minisat_lbool
#define HTYPE_lbool LBool

#unsafe minisat_new,              0, io(solver)
#unsafe minisat_delete,           1(solver), io(unit)
#unsafe minisat_newVar,           1(solver), io(var)
#unsafe minisat_newLit,           1(solver), io(lit)
#unsafe minisat_mkLit,            1(var), lit
#unsafe minisat_mkLit_args,       2(var, int), lit
#unsafe minisat_negate,           1(lit), lit
#unsafe minisat_var,              1(lit), var
#unsafe minisat_sign,             1(lit), bool
#unsafe minisat_addClause,        3(solver, int, ptr(lit)), io(bool)
#unsafe minisat_addClause_begin,  1(solver), io(unit)
#unsafe minisat_addClause_addLit, 2(solver, lit), io(unit)
#unsafe minisat_addClause_commit, 1(solver), io(bool)
#unsafe minisat_simplify,         1(solver), io(bool)
#safe minisat_solve,              3(solver, int, ptr(lit)), io(bool)
#unsafe minisat_solve_begin,      1(solver), io(unit)
#unsafe minisat_solve_addLit,     2(solver, lit), io(unit)
#safe minisat_solve_commit,       1(solver), io(bool)
#safe minisat_limited_solve_commit,       1(solver), io(lbool)

#safe minisat_interrupt,          1(solver), io(unit)
#safe minisat_clearInterrupt,     1(solver), io(unit)

#unsafe minisat_okay,             1(solver), io(bool)
#unsafe minisat_setPolarity,      3(solver, var, int), io(unit)
#unsafe minisat_setDecisionVar,   3(solver, var, int), io(unit)
#unsafe minisat_value_Var,        2(solver, var), io(lbool)
#unsafe minisat_value_Lit,        2(solver, lit), io(lbool)
#unsafe minisat_modelValue_Var,   2(solver, var), io(lbool)
#unsafe minisat_modelValue_Lit,   2(solver, lit), io(lbool)

#unsafe minisat_get_l_True,       0, lbool
#unsafe minisat_get_l_False,      0, lbool
#unsafe minisat_get_l_Undef,      0, lbool

-- // Simpsolver methods:
#unsafe minisat_setFrozen,        3(solver, var, bool), io(unit)
#unsafe minisat_isEliminated,     2(solver, var), io(bool)
#unsafe minisat_eliminate,        2(solver, bool), io(bool)

#unsafe minisat_num_assigns,      1(solver), io(int)
#unsafe minisat_num_clauses,      1(solver), io(int)
#unsafe minisat_num_learnts,      1(solver), io(int)
#unsafe minisat_num_vars,         1(solver), io(int)
#unsafe minisat_num_freeVars,     1(solver), io(int)
#unsafe minisat_num_conflicts,    1(solver), io(int)

#unsafe minisat_conflict_len,     1(solver), io(int)
#unsafe minisat_conflict_nthLit,  2(solver, int), io(lit)
#unsafe minisat_set_verbosity,    2(solver, int), io(unit)
