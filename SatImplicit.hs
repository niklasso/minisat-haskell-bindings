{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
module SatImplicit
  -- a general implicit interface
  ( withNewSolver -- :: MonadSat m => (forall s . (?solver :: Solver m s) => m a) -> m a
  , runWithSolver -- :: (forall s t . (?solver :: Solver (Sat t) s) => Sat t a) -> a
  , newLit        -- :: (MonadSat m, ?solver :: Solver m s) => m (Lit s)
  , addClause     -- :: (MonadSat m, ?solver :: Solver m s) => [Lit s] -> m Bool
  , simplify      -- :: (MonadSat m, ?solver :: Solver m s) => m ()
  , solve         -- :: (MonadSat m, ?solver :: Solver m s) => [Lit s] -> m Bool
  , value         -- :: (MonadSat m, ?solver :: Solver m s) => Lit s -> m (Maybe Bool)
  , modelValue    -- :: (MonadSat m, ?solver :: Solver m s) => Lit s -> m (Maybe Bool)
  , conflict      -- :: (MonadSat m, ?solver :: Solver m s) => m [Lit s]
  )
 where

import qualified Sat as S

import Sat
  ( Solver
  , Lit
  , Sat
  , MonadSat
  )

----------------------------------------------------------------------------
-- using implicit arguments

withNewSolver :: MonadSat m => (forall s . (?solver :: Solver m s) => m a) -> m a
withNewSolver m = S.withNewSolver (\s -> let ?solver = s in m)

runWithSolver :: (forall s t . (?solver :: Solver (Sat t) s) => Sat t a) -> a
runWithSolver m = S.run (S.withNewSolver (\s -> let ?solver = s in m))

newLit :: (MonadSat m, ?solver :: Solver m s) => m (Lit s)
newLit = S.newLit ?solver

addClause :: (MonadSat m, ?solver :: Solver m s) => [Lit s] -> m Bool
addClause xs = S.addClause ?solver xs

simplify :: (MonadSat m, ?solver :: Solver m s) => m ()
simplify = S.simplify ?solver

solve :: (MonadSat m, ?solver :: Solver m s) => [Lit s] -> m Bool
solve xs = S.solve ?solver xs

value :: (MonadSat m, ?solver :: Solver m s) => Lit s -> m (Maybe Bool)
value x = S.value ?solver x

modelValue :: (MonadSat m, ?solver :: Solver m s) => Lit s -> m (Maybe Bool)
modelValue x = S.modelValue ?solver x

conflict :: (MonadSat m, ?solver :: Solver m s) => m [Lit s]
conflict = S.conflict ?solver

----------------------------------------------------------------------------
