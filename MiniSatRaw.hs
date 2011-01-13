{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.Ptr     ( Ptr, nullPtr )
import Foreign.C.Types ( CInt )

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.List( nub, (\\) )
import System.IO
import Control.Exception ( finally )

----------------------------------------------------------------------------

data Check
  = Result
  | Model
  | Conflict
 deriving ( Show, Eq )

instance Arbitrary Check where
  arbitrary = elements [Result,Model,Conflict]
  
  shrink Result = []
  shrink _      = [Result]

data Command
  = NewLit
  | AddClause [Int]
  | Solve [Int] [Check]
  | Value Int
 deriving ( Show, Eq )

arbCommand :: Int -> Gen Command
arbCommand n =
  frequency $
  [ (3, return NewLit)
  ] ++
  concat
  [ [ (9 `min` n, AddClause  `fmap` arbClause)
    , (1, (\xs -> Solve xs [Result,Model,Conflict]) `fmap` arbAssumps)
    , (1, Value      `fmap` arbLit)
    ]
  | n > 0
  ]
 where
  arbLit = elements ([-n..n]\\[0])

  arbClause =
    frequency
    [ (500, do k <- frequency [(1, return 1), (3, return 2), (10, choose (3,7))]
               sequence [ arbLit | i <- [(1::Int)..k] ])
    , (1, do return [])
    ]
    
  arbAssumps =
    do k <- choose (0,7)
       sequence [ arbLit | i <- [(1::Int)..k] ]
    
shrinkCommand :: Command -> [Command]
shrinkCommand (AddClause xs) = [ AddClause (filter (/=0) xs') | xs' <- shrink xs ]
shrinkCommand (Solve xs cx)  = [ Solve (filter (/=0) xs') cx | xs' <- shrink xs ]
                            ++ [ Solve xs cx' | cx' <- shrink cx ]
shrinkCommand _              = []

data Program
  = Program [Command]
 deriving ( Show, Eq )

arbProgram :: Int -> Int -> Gen [Command]
arbProgram 0 _ = return []
arbProgram n l =
  do c  <- arbCommand l
     cs <- arbProgram (n-1) (if c == NewLit then l+1 else l)
     return (c:cs)

instance Arbitrary Program where
  arbitrary =
    do (Program . (++ [Solve [] [Result,Model,Conflict]])) `fmap` arbProgram 400 0

  shrink (Program cs) = map Program $
    removeOne cs ++
    shrinkOne cs
   where
    removeOne cs =
      [ take k cs ++ drop (k+1) (filter (/=Value 0) (adapt c (take k cs) cs))
      | (k,c) <- reverse ([0..] `zip` cs)
      ]
     where
      adapt NewLit cs' cs = map (dec (length (filter (==NewLit) cs'))) cs
      adapt _      _   cs = cs
      
      dec n (AddClause xs) = AddClause (decs n xs)
      dec n (Solve xs cx)  = Solve (decs n xs) cx
      dec n (Value x)      = Value (decf n x)
      dec n c              = c
      
      decs n xs = filter (/=0) (map (decf n) xs)
      decf n x  = if abs x > n then if x > 0 then x-1 else x+1 else x

    shrinkOne cs =
      [ take k cs ++ [c'] ++ drop (k+1) cs
      | (k,c) <- [0..] `zip` cs
      , c' <- shrinkCommand (cs !! k)
      ]

satisfiable :: [[Int]] -> IO Bool
satisfiable p =
  do s  <- newSolver
     xs <- sequence [ newLit s | i <- [1..n] ]
     sequence_ [ addClause s (map (lit xs) c) | c <- p ]
     b <- solve s []
     deleteSolver s
     return b
 where
  n = maximum (1:nub [ abs i | c <- p, i <- c ])

lit :: [Lit] -> Int -> Lit
lit xs k | k > 0     = xs !! (k-1)
         | otherwise = neg (xs !! (abs k - 1))

til :: [Lit] -> Lit -> Int
til xs x = head [ i | (i,y) <- ([1..] `zip` xs) ++ ([-1,-2..] `zip` map neg xs), x == y]

check :: Solver -> [[Int]] -> [Lit] -> [Command] -> IO (Maybe String)
check s p xs [] =
  do return Nothing
check s p xs (c:cs) =
  case c of
    NewLit ->
      do x <- newLit s
         check s p (xs ++ [x]) cs

    AddClause is ->
      do addClause s (map (lit xs) is)
         check s (is:p) xs cs
    
    Solve is cx ->
      do b <- solve s (map (lit xs) is)
         ok <- checks b cx
         case ok of
           Nothing -> check s p xs cs
           _       -> return ok
     where
      checks b (Result:cx) =
        do b' <- satisfiable (p ++ [[i] | i <- is])
           if b /= b' then
             return (Just ("Wrong result: program says " ++ show b ++", spec says " ++ show b'))
            else
             checks b cx
             
      checks True (Model:cx) =
        do vs <- sequence [ modelValue s x | x <- xs ]
           b' <- satisfiable (p ++ [[i] | i <- is]
                                ++ [[if v == Just True then i else -i] | (i,v) <- [1..] `zip` vs])
           if not b' then
             return (Just ("Wrong model"))
            else
             checks True cx
             
      checks False (Conflict:cx) =
        do cfl <- conflict s
           b' <- satisfiable (p ++ [[-(til xs x)] | x <- cfl])
           if b' then
             return (Just ("Wrong conflict: " ++ show (map (til xs) cfl)))
            else
             checks False cx
       where
        len = length . nub . map abs
   
      checks b (_:cx) = checks b cx
      checks _ _      = return Nothing
  
    Value x ->
      do v <- value s (lit xs x)
         case v of
           Just a ->
             do b <- satisfiable (p ++ [[if a then -x else x]])
                if b then
                  return (Just ("Wrong value: " ++ show v))
                 else
                  check s p xs cs
           
           Nothing -> check s p xs cs

prop_Program (Program p) =
  monadicIO $
    do ok <- run $
               do s <- newSolver
                  ok <- check s [] [] p
                  deleteSolver s
                  return ok
       case ok of
         Just err -> monitor (whenFail (putStrLn err))
         _        -> return ()
       assert (ok == Nothing)

prop_Satisfiable xss =
  monadicIO $
    do x <- pick (elements (concat xss'))
       b <- run $
              do a  <- satisfiable xss'
                 a0 <- satisfiable (concatMap (assume x) xss')
                 a1 <- satisfiable (concatMap (assume (-x)) xss')
                 return (a == (a0 || a1))
       assert b
   where
    xss' = map (filter (/=0)) xss

    assume x xs | x `elem` xs = []
                | otherwise   = [filter (/=(-x)) xs]
    
-- FIXME: previously used this function. What does it do?
--  do b <- quickCheck' prop_Program

main =
  do r <- quickCheckResult prop_Program
     case r of
       Success _ -> main
       _         -> return ()

----------------------------------------------------------------------------

test =
  do s <- newSolver
     x <- newLit s
     y <- newLit s
     
     addClause s [x, y]
     addClause s [neg x, neg y]

     b <- solve s []
     printResult s x y b
     
     b <- solve s [x]
     printResult s x y b
     
     b <- solve s [x,y]
     printResult s x y b
     
     deleteSolver s

printResult s x y True =
  do a <- modelValue s x
     b <- modelValue s y
     putStrLn ("SAT! x=" ++ show a ++ ", y=" ++ show b)

printResult s x y False =
  do cnf <- conflict s
     putStrLn ("UNSAT! " ++ show cnf)

----------------------------------------------------------------------------

withNewSolver :: (Solver -> IO a) -> IO a
withNewSolver h =
  do s <- newSolver
     h s `finally` deleteSolver s

newSolver :: IO Solver
newSolver = solver_new
--newSolver = do s <- solver_new; solver_set_verbosity s 0; return s

deleteSolver :: Solver -> IO ()
deleteSolver = solver_delete

newLit :: Solver -> IO Lit
newLit = solver_newLit

neg :: Lit -> Lit
neg = solver_negate

addClause :: Solver -> [Lit] -> IO ()
addClause s xs =
  do solver_addClause_begin s
     sequence_ [ solver_addClause_addLit s x | x <- xs ]
     solver_addClause_commit s

simplify :: Solver -> IO ()
simplify = solver_simplify

{-
solve :: Solver -> [Lit] -> Model a -> IO (Either [Lit] a)
-}

solve :: Solver -> [Lit] -> IO Bool
solve s xs =
  do solver_solve_begin s
     sequence_ [ solver_solve_addLit s x | x <- xs ]
     solver_solve_commit s

value, modelValue :: Solver -> Lit -> IO (Maybe Bool)
(value,modelValue) = (get solver_value_Lit, get solver_modelValue_Lit)
 where
  get f s x = mbool `fmap` f s x

  mbool b 
    | b == solver_get_l_False = Just False
    | b == solver_get_l_True  = Just True
    | otherwise               = Nothing

conflict :: Solver -> IO [Lit]
conflict s =
  do n <- solver_conflict_len s
     sequence [ solver_conflict_nthLit s i | i <- [0..n-1] ]
     
----------------------------------------------------------------------------

main_raw =
  do s <- solver_new
     solver_set_verbosity s 0
     x <- solver_newLit s
     y <- solver_newLit s
     
     solver_addClause_begin s
     solver_addClause_addLit s x
     solver_addClause_addLit s y
     solver_addClause_commit s
     
     solver_addClause_begin s
     solver_addClause_addLit s (solver_negate x)
     solver_addClause_addLit s (solver_negate y)
     solver_addClause_commit s
 
     b <- solver_solve s 0 nullPtr
     printResult s x y b
     
     solver_solve_begin s
     solver_solve_addLit s x
     b <- solver_solve_commit s
     printResult s x y b
     
     solver_solve_begin s
     solver_solve_addLit s x
     solver_solve_addLit s y
     b <- solver_solve_commit s
     printResult s x y b
     
     solver_delete s

printResult_raw s x y True =
  do a <- solver_modelValue_Lit s x
     b <- solver_modelValue_Lit s y
     putStrLn ("SAT! x=" ++ show a ++ ", y=" ++ show b)

printResult_raw s x y False =
  do n <- solver_conflict_len s
     cnf <- sequence
              [ solver_conflict_nthLit s i
              | i <- [0..n-1]
              ]
     putStrLn ("UNSAT! " ++ show cnf)

----------------------------------------------------------------------------

newtype Solver = MkSolver (Ptr ())
newtype Var    = MkVar CInt  deriving ( Eq, Ord )
newtype Lit    = MkLit CInt  deriving ( Eq, Ord )
newtype LBool  = MkLBool Int deriving (Eq, Ord)

instance Show Var where
  show (MkVar n) = 'v' : show n

instance Show Lit where
  show x = (if solver_sign x then "~" else "") ++ show (solver_var x) 

instance Show LBool where
  show b
    | b == solver_get_l_False = "False"
    | b == solver_get_l_True  = "True"
    | otherwise               = "Undef"

foreign import ccall unsafe solver_new :: IO Solver
foreign import ccall unsafe solver_delete :: Solver -> IO ()
foreign import ccall unsafe solver_newVar :: Solver -> IO Var
foreign import ccall unsafe solver_newLit :: Solver -> IO Lit
foreign import ccall unsafe solver_mkLit :: Var -> Lit
foreign import ccall unsafe solver_mkLit_args :: Var -> Bool -> Lit
foreign import ccall unsafe solver_negate :: Lit -> Lit
foreign import ccall unsafe solver_var :: Lit -> Var
foreign import ccall unsafe solver_sign :: Lit -> Bool
foreign import ccall unsafe solver_addClause :: Solver -> Int -> Ptr Lit -> IO ()
foreign import ccall unsafe solver_addClause_begin :: Solver -> IO ()
foreign import ccall unsafe solver_addClause_addLit :: Solver -> Lit -> IO ()
foreign import ccall unsafe solver_addClause_commit :: Solver -> IO ()
foreign import ccall unsafe solver_simplify :: Solver -> IO ()
foreign import ccall unsafe solver_solve :: Solver -> Int -> Ptr Lit -> IO Bool
foreign import ccall unsafe solver_solve_begin :: Solver -> IO ()
foreign import ccall unsafe solver_solve_addLit :: Solver -> Lit -> IO ()
foreign import ccall unsafe solver_solve_commit :: Solver -> IO Bool
foreign import ccall unsafe solver_okay :: Solver -> IO Bool
foreign import ccall unsafe solver_setPolarity :: Solver -> Var -> Bool -> IO ()
foreign import ccall unsafe solver_setDecisionVar :: Solver -> Var -> Bool -> IO ()
foreign import ccall unsafe solver_get_l_True :: LBool
foreign import ccall unsafe solver_get_l_False :: LBool
foreign import ccall unsafe solver_get_l_Undef :: LBool
foreign import ccall unsafe solver_value_Var :: Solver -> Var -> IO LBool
foreign import ccall unsafe solver_value_Lit :: Solver -> Lit -> IO LBool
foreign import ccall unsafe solver_modelValue_Var :: Solver -> Var -> IO LBool
foreign import ccall unsafe solver_modelValue_Lit :: Solver -> Lit -> IO LBool

-- orkar...
{-
int solver_num_assigns(solver *s);
int solver_num_clauses(solver *s);     
int solver_num_learnts(solver *s);     
int solver_num_vars(solver *s);  
int solver_num_freeVars(solver *s);
int solver_num_conflicts(solver *s);
-}

foreign import ccall unsafe solver_conflict_len :: Solver -> IO Int
foreign import ccall unsafe solver_conflict_nthLit :: Solver -> Int -> IO Lit
foreign import ccall unsafe solver_set_verbosity :: Solver -> Int -> IO ()
