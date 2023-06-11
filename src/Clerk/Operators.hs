module Clerk.Operators where

import Clerk.Expression (BinaryOperator (..), Expr (..))
import Clerk.Formula (Formula (..), ToFormula (..))
import Clerk.Internal (UnsafeChangeType (unsafeChangeType))
import Clerk.Reference (Ref)
import Data.Kind (Type)

mkOp2 :: (ToFormula a, ToFormula b) => BinaryOperator -> a -> b -> Formula t
mkOp2 f c1 c2 = Formula $ EBinaryOp f (unFormula $ toFormula c1) (unFormula $ toFormula c2)

mkNumOp2 :: (Num t, ToFormula a, ToFormula b) => BinaryOperator -> a -> b -> Formula t
mkNumOp2 = mkOp2

data Range

-- | Construct a range expression
(.:) :: forall (a :: Type) (b :: Type). Ref a -> Ref b -> Formula Range
(.:) a b = Formula $ ERange (unsafeChangeType a) (unsafeChangeType b)

infixr 5 .:

-- | Convert a value to a formula
val :: Show a => a -> Formula a
val a = Formula $ EValue a

-- | A type for numeric operators
type NumOperator a b c d e = (ToFormula (d a), ToFormula (e b)) => d a -> e b -> Formula c

-- | Construct an addition expression like @A1 + B1@
(.+) :: Num a => NumOperator a a a d e
(.+) = mkNumOp2 OpAdd

infixl 6 .+

-- | Construct a subtraction expression like @A1 - B1@
(.-) :: Num a => NumOperator a a a d e
(.-) = mkNumOp2 OpSubtract

infixl 6 .-

-- | Construct a division expression like @A1 / B1@
(./) :: Fractional a => NumOperator a a a d e
(./) = mkNumOp2 OpDivide

infixl 7 ./

-- | Construct a multiplication expression like @A1 * B1@
(.*) :: Num a => NumOperator a a a d e
(.*) = mkNumOp2 OpMultiply

infixl 6 .*

-- | Construct an exponentiation expression like @A1 ^ B1@
(.^) :: (Num a, Integral b) => NumOperator a b a d e
(.^) = mkNumOp2 OpPower

infixr 8 .^

-- | Construct an exponentiation expression like @A1 ^ B1@ with 'Fractional' base
(.^^) :: (Fractional a, Integral b) => NumOperator a b a d e
(.^^) = mkNumOp2 OpPower

infixr 8 .^^

-- | Construct an exponentiation expression like @A1 ^ B1@ with 'Floating' base
(.**) :: (Floating a) => NumOperator a a a d e
(.**) = mkNumOp2 OpPower

infixr 8 .**

type BoolOperator a b c = (Ord a, ToFormula (b a), ToFormula (c a)) => b a -> c a -> Formula Bool

mkBoolOp2 :: (Ord a, ToFormula (b a), ToFormula (c a)) => BinaryOperator -> b a -> c a -> Formula Bool
mkBoolOp2 f c1 c2 = Formula $ EBinaryOp f (unFormula $ toFormula c1) (unFormula $ toFormula c2)

-- | Construct a @less-than@ expression like @A1 < B1@
(.<) :: BoolOperator a b c
(.<) = mkBoolOp2 OpLT

infix 4 .<

-- | Construct a @greater-than@ expression like @A1 > B1@
(.>) :: BoolOperator a b c
(.>) = mkBoolOp2 OpGT

infix 4 .>

-- | Construct a @less-than-or-equal-to@ expression like @A1 <= B1@
(.<=) :: BoolOperator a b c
(.<=) = mkBoolOp2 OpLEQ

infix 4 .<=

-- | Construct a @greater-than-or-equal-to@ expression like @A1 <= B1@
(.>=) :: BoolOperator a b c
(.>=) = mkBoolOp2 OpGEQ

infix 4 .>=

-- | Construct a @equal-to@ expression like @A1 = B1@
(.=) :: BoolOperator a b c
(.=) = mkBoolOp2 OpEQ

infix 4 .=

-- | Construct a @not-equal-to@ expression like @A1 <> B1@
(.<>) :: BoolOperator a b c
(.<>) = mkBoolOp2 OpNEQ

infix 4 .<>
