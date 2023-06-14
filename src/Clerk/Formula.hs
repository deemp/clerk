module Clerk.Formula where

import Clerk.Coordinates (Coords)
import Clerk.Expression (Expr (ERef))
import Clerk.Internal (UnsafeChangeType (unsafeChangeType))
import Clerk.Reference (Ref (Ref))

-- | Formula
newtype Formula t = Formula {_formula :: Expr t}
  deriving newtype (UnsafeChangeType)

-- | Something that can be turned into a formula
class ToFormula a where
  toFormula :: a -> Formula b

instance ToFormula (Ref a) where
  toFormula :: Ref a -> Formula b
  toFormula (Ref c) = Formula $ ERef (Ref c)

-- | Convert a reference to a formula
formulaRef :: Ref a -> Formula a
formulaRef = toFormula

instance ToFormula Coords where
  toFormula :: Coords -> Formula t
  toFormula c = Formula $ ERef (Ref c)

instance ToFormula (Expr a) where
  toFormula :: Expr a -> Formula b
  toFormula = Formula . unsafeChangeType

instance ToFormula (Formula a) where
  toFormula :: Formula a -> Formula b
  toFormula (Formula f) = Formula $ unsafeChangeType f

instance Show (Expr t) => Show (Formula t) where
  show :: Formula t -> String
  show (Formula f) = show f