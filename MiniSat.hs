{-# LANGUAGE ForeignFunctionInterface #-}
module MiniSat where

import Foreign.Ptr     ( Ptr, nullPtr )
import Foreign.C.Types ( CInt )
import Control.Exception ( finally )

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
    | b == minisat_get_l_False = Just False
    | b == minisat_get_l_True  = Just True
    | otherwise               = Nothing

conflict :: Solver -> IO [Lit]
conflict s =
  do n <- minisat_conflict_len s
     sequence [ minisat_conflict_nthLit s i | i <- [0..n-1] ]
     
-- TODO: are these good enough? Also, add functions: negate, and, or, etc.
l_True  = minisat_get_l_True
l_False = minisat_get_l_False
l_Undef = minisat_get_l_Undef

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
    | b == minisat_get_l_False = "False"
    | b == minisat_get_l_True  = "True"
    | otherwise                = "Undef"

foreign import ccall unsafe minisat_new :: IO Solver
foreign import ccall unsafe minisat_delete :: Solver -> IO ()
foreign import ccall unsafe minisat_newVar :: Solver -> IO Var
foreign import ccall unsafe minisat_newLit :: Solver -> IO Lit
foreign import ccall unsafe minisat_mkLit :: Var -> Lit
foreign import ccall unsafe minisat_mkLit_args :: Var -> Bool -> Lit
foreign import ccall unsafe minisat_negate :: Lit -> Lit
foreign import ccall unsafe minisat_var :: Lit -> Var
foreign import ccall unsafe minisat_sign :: Lit -> Bool
foreign import ccall unsafe minisat_addClause :: Solver -> Int -> Ptr Lit -> IO Bool
foreign import ccall unsafe minisat_addClause_begin :: Solver -> IO ()
foreign import ccall unsafe minisat_addClause_addLit :: Solver -> Lit -> IO ()
foreign import ccall unsafe minisat_addClause_commit :: Solver -> IO Bool
foreign import ccall unsafe minisat_simplify :: Solver -> IO Bool
foreign import ccall unsafe minisat_solve :: Solver -> Int -> Ptr Lit -> IO Bool
foreign import ccall unsafe minisat_solve_begin :: Solver -> IO ()
foreign import ccall unsafe minisat_solve_addLit :: Solver -> Lit -> IO ()
foreign import ccall unsafe minisat_solve_commit :: Solver -> IO Bool
foreign import ccall unsafe minisat_okay :: Solver -> IO Bool
foreign import ccall unsafe minisat_setPolarity :: Solver -> Var -> LBool -> IO ()
foreign import ccall unsafe minisat_setDecisionVar :: Solver -> Var -> Bool -> IO ()
foreign import ccall unsafe minisat_get_l_True :: LBool
foreign import ccall unsafe minisat_get_l_False :: LBool
foreign import ccall unsafe minisat_get_l_Undef :: LBool
foreign import ccall unsafe minisat_value_Var :: Solver -> Var -> IO LBool
foreign import ccall unsafe minisat_value_Lit :: Solver -> Lit -> IO LBool
foreign import ccall unsafe minisat_modelValue_Var :: Solver -> Var -> IO LBool
foreign import ccall unsafe minisat_modelValue_Lit :: Solver -> Lit -> IO LBool

-- // SimpSolver methods:
foreign import ccall unsafe minisat_setFrozen :: Solver -> Var -> Bool -> IO ()
foreign import ccall unsafe minisat_isEliminated :: Solver -> Var -> IO Bool
foreign import ccall unsafe minisat_eliminate :: Solver -> Bool -> IO Bool

-- orkar...
{-
int minisat_num_assigns(solver *s);
int minisat_num_clauses(solver *s);     
int minisat_num_learnts(solver *s);     
int minisat_num_vars(solver *s);  
int minisat_num_freeVars(solver *s);
int minisat_num_conflicts(solver *s);
-}

foreign import ccall unsafe minisat_conflict_len :: Solver -> IO Int
foreign import ccall unsafe minisat_conflict_nthLit :: Solver -> Int -> IO Lit
foreign import ccall unsafe minisat_set_verbosity :: Solver -> Int -> IO ()
