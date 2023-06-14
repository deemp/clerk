module Clerk.Expression where

import Clerk.Internal (UnsafeChangeType (unsafeChangeType))
import Clerk.Reference (Ref (Ref))
import Data.String (IsString)
import qualified Data.Text as T
import Unsafe.Coerce (unsafeCoerce)

-- | Name of a function like @SUM@
newtype FunctionName = FunctionName {_functionName :: T.Text} deriving newtype (IsString)

-- | Expressions
data Expr t
  = EBinaryOperation {_binaryOperator :: BinaryOperator, _argLeft :: Expr t, _argRight :: Expr t}
  | EFunction {_fName :: FunctionName, _fArgs :: [Expr t]}
  | ERef {_ref :: Ref t}
  | ERange {_refUpperLeft :: Ref t, _refLowerRight :: Ref t}
  | EValue {_value :: t}
  | EUnaryOperation {_unaryOperator :: UnaryOperator, _arg :: Expr t}

data BinaryOperator
  = OpAdd
  | OpSubtract
  | OpMultiply
  | OpDivide
  | OpPower
  | OpLT
  | OpGT
  | OpLEQ
  | OpGEQ
  | OpEQ
  | OpNEQ

data UnaryOperator
  = OpNeg

instance UnsafeChangeType Expr where
  unsafeChangeType :: Expr b -> Expr c
  unsafeChangeType (EBinaryOperation a b c) = EBinaryOperation a (unsafeChangeType b) (unsafeChangeType c)
  unsafeChangeType (ERef (Ref a)) = ERef (Ref a)
  unsafeChangeType (EFunction n args) = EFunction n (unsafeChangeType <$> args)
  unsafeChangeType (ERange l r) = ERange (unsafeChangeType l) (unsafeChangeType r)
  unsafeChangeType (EUnaryOperation u v) = EUnaryOperation u (unsafeCoerce v)
  unsafeChangeType (EValue v) = EValue (unsafeCoerce v)

instance Show t => Show (Expr t) where
  show :: Expr t -> String
  show (EValue v) = show v
  show _ = error "Shouldn't be accessed for other constructors"
