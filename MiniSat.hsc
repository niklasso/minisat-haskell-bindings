{-# LANGUAGE ForeignFunctionInterface #-}
module MiniSat where

import Foreign.Ptr     ( Ptr, nullPtr )
import Foreign.C.Types ( CInt )
import Control.Exception ( finally )

#define Minisat_Struct(x) struct { x f; }
#include "minisat.h"
#include "hsc-magic.h"

withNewSolver :: (Solver -> IO a) -> IO a
withNewSolver h =
  do s <- newSolver
     h s `finally` deleteSolver s

newSolver :: IO Solver
newSolver = minisat_new
--newSolver = do s <- minisat_new; minisat_set_verbosity s 0; return s

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
     
-- TODO: are these good enough? Also, add functions: negate, and, or, etc.
l_True  = MkLBool $ #const minisat_l_True.f
l_False = MkLBool $ #const minisat_l_False.f
l_Undef = MkLBool $ #const minisat_l_Undef.f

----------------------------------------------------------------------------

newtype Solver = MkSolver (Ptr ())
newtype Var    = MkVar CInt  deriving ( Eq, Ord )
newtype Lit    = MkLit CInt  deriving ( Eq, Ord )
newtype LBool  = MkLBool Int deriving ( Eq, Ord )

instance Show Var where
  show (MkVar n) = 'v' : show n

instance Show Lit where
  show x = (if minisat_sign x then "~" else "") ++ show (minisat_var x) 

instance Show LBool where
  show b
    | b == l_False = "False"
    | b == l_True  = "True"
    | otherwise    = "Undef"

#define TYPE_solver(f) f(minisat_solver*, Solver)
#define TYPE_void(f) f(void, ())
#define TYPE_bool(f) f(minisat_bool, Bool)
#define TYPE_lit(f) f(minisat_Lit, Lit)
#define TYPE_lits(f) f(minisat_Lit*, Ptr Lit)
#define TYPE_int(f) f(int, Int)
#define TYPE_var(f) f(minisat_Var, Var)
#define TYPE_lbool(f) f(minisat_lbool, LBool)

#unsafe io, solver, minisat_new
#unsafe io, void, minisat_delete, solver
#unsafe io, var, minisat_newVar, solver
#unsafe io, lit, minisat_newLit, solver
#unsafe pure, lit, minisat_mkLit, var
#unsafe pure, lit, minisat_mkLit_args, var, int
#unsafe pure, lit, minisat_negate, lit
#unsafe pure, var, minisat_var, lit
#unsafe pure, bool, minisat_sign, lit
#unsafe io, bool, minisat_addClause, solver, int, lits
#unsafe io, void, minisat_addClause_begin, solver
#unsafe io, void, minisat_addClause_addLit, solver, lit
#unsafe io, bool, minisat_addClause_commit, solver
#unsafe io, bool, minisat_simplify, solver
#safe io, bool, minisat_solve, solver, int, lits
#unsafe io, void, minisat_solve_begin, solver
#unsafe io, void, minisat_solve_addLit, solver, lit
#safe io, bool, minisat_solve_commit, solver
#unsafe io, bool, minisat_okay, solver
#unsafe io, void, minisat_setPolarity, solver, var, int
#unsafe io, void, minisat_setDecisionVar, solver, var, int
#unsafe io, lbool, minisat_value_Var, solver, var
#unsafe io, lbool, minisat_value_Lit, solver, lit
#unsafe io, lbool, minisat_modelValue_Var, solver, var
#unsafe io, lbool, minisat_modelValue_Lit, solver, lit

-- // Simpsolver methods:
#unsafe io, void, minisat_setFrozen, solver, var, bool
#unsafe io, bool, minisat_isEliminated, solver, var
#unsafe io, bool, minisat_eliminate, solver, bool

#unsafe io, int, minisat_num_assigns, solver
#unsafe io, int, minisat_num_clauses, solver
#unsafe io, int, minisat_num_learnts, solver
#unsafe io, int, minisat_num_vars, solver
#unsafe io, int, minisat_num_freeVars, solver
#unsafe io, int, minisat_num_conflicts, solver

#unsafe io, int, minisat_conflict_len, solver
#unsafe io, lit, minisat_conflict_nthLit, solver, int
#unsafe io, void, minisat_set_verbosity, solver, int
