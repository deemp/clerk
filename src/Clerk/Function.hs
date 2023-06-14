module Clerk.Function where

import Clerk.Expression (Expr (EFunction), FunctionName)
import Clerk.Formula (Formula (..), ToFormula (..))
import Clerk.Internal (UnsafeChangeType (unsafeChangeType))

class MakeFunction t where
  makeFunction :: FunctionName -> [Formula s] -> t

instance MakeFunction (Formula a) where
  makeFunction :: FunctionName -> [Formula s] -> Formula a
  makeFunction name args = Formula $ EFunction name (unsafeChangeType . _formula <$> args)

instance (Foldable f, MakeFunction t, ToFormula a) => MakeFunction (f a -> t) where
  makeFunction :: FunctionName -> [Formula s] -> f a -> t
  makeFunction name args xs =
    makeFunction
      name
      ((unsafeChangeType . toFormula <$> args) ++ foldMap ((: []) . unsafeChangeType . toFormula) xs)

-- | Construct a function like @SUM(A1,B1)@
fun :: MakeFunction t => FunctionName -> t
fun n = makeFunction n []