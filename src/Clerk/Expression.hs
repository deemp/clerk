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
  = EBinaryOp {binOp :: BinaryOperator, arg1 :: Expr t, arg2 :: Expr t}
  | EFunction {fName :: FunctionName, fArgs :: [Expr t]}
  | ERef {r :: Ref t}
  | ERange {ref1 :: Ref t, ref2 :: Ref t}
  | EValue {value :: t}
  | EUnaryOp {unaryOp :: UnaryOp, arg :: Expr t}

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

data UnaryOp
  = OpNeg

instance UnsafeChangeType Expr where
  unsafeChangeType :: Expr b -> Expr c
  unsafeChangeType (EBinaryOp a b c) = EBinaryOp a (unsafeChangeType b) (unsafeChangeType c)
  unsafeChangeType (ERef (Ref a)) = ERef (Ref a)
  unsafeChangeType (EFunction n args) = EFunction n (unsafeChangeType <$> args)
  unsafeChangeType (ERange l r) = ERange (unsafeChangeType l) (unsafeChangeType r)
  unsafeChangeType (EUnaryOp u v) = EUnaryOp u (unsafeCoerce v)
  unsafeChangeType (EValue v) = EValue (unsafeCoerce v)

instance Show t => Show (Expr t) where
  show :: Expr t -> String
  show (EValue v) = show v
  show _ = error "Shouldn't be accessed for other constructors"
