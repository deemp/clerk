{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clerk.AddressTyped where

import Clerk.Coordinates
import Clerk.Internal
import Clerk.Symbols
import Codec.Xlsx (ColumnIndex, RowIndex)
import Data.Char
import Data.Data
import Data.Default
import qualified Data.Text as T
import Fcf (Eval, Exp, Fst, Snd, type (++))
import Fcf.Class.Foldable
import Fcf.Data.List (DropWhile, Span, TakeWhile)
import GHC.Base
import GHC.TypeLits as TL

data CheckColumnCharExp :: Char -> Exp Bool

type instance Eval (CheckColumnCharExp c) = CharBetween c 'A' 'Z'

type P2 = Eval (All CheckColumnCharExp (ToChars "ABC"))

type ParseFailureMsg = Text "Failed to parse the address.\n"
type ParseFailureStructureMsg = Text "The address must be a column index and a row number.\n"
type RuleColumnIndex = Text "The column index must have one or more uppercase letters.\n"
type RuleRowIndexFirstDigit = Text "The first digit of the row number must be between 1 and 9.\n"
type RuleRowIndex = Text "The row number must have one or more digits.\n"
type CurrentColumn column = (Text "column: " :<>: Text (FromChars column) :<>: Text "\n")
type CurrentRow row = (Text "row: " :<>: Text (FromChars row) :<>: Text "\n")
type CurrentRest rest = (Text "rest: " :<>: Text (FromChars rest) :<>: Text "\n")
type CurrentState column row rest = CurrentColumn column :<>: CurrentRow row :<>: CurrentRest rest

type family CheckedAddress3 (column :: [Char]) (row :: [Char]) (rest :: [Char]) :: ([Char], [Char], Constraint) where
  CheckedAddress3 '[] row rest = TypeError (ParseFailureMsg :<>: RuleColumnIndex :<>: CurrentState '[] row rest)
  CheckedAddress3 column '[] rest = TypeError (ParseFailureMsg :<>: RuleRowIndex :<>: CurrentState column '[] rest)
  CheckedAddress3 column row '[] = '(column, row, (KnownSymbol (FromChars column), KnownNat (FromCharsNatural row)))
  CheckedAddress3 column row rest = TypeError (ParseFailureMsg :<>: ParseFailureStructureMsg :<>: RuleColumnIndex :<>: RuleRowIndex :<>: CurrentState '[] row rest)

data CheckRowCharExp :: Char -> Exp Bool
data CheckRowFirstCharExp :: Char -> Exp Bool

type instance Eval (CheckRowCharExp c) = CharBetween c '0' '9'
type instance Eval (CheckRowFirstCharExp c) = CharBetween c '1' '9'

type TakeColumnChars s = Eval (Span CheckColumnCharExp s)
type TakeRowFirstChar s = Eval (Span CheckRowFirstCharExp (Eval (Snd (TakeColumnChars s))))
type TakeRowChars s = Eval (Span CheckRowCharExp (Eval (Snd (TakeRowFirstChar s))))
type TakeRestChars s = Eval (Snd (TakeRowChars s))

type family CheckedAddress2 (s :: [Char]) :: ([Char], [Char], Constraint) where
  CheckedAddress2 s =
    CheckedAddress3
      (Eval (Fst (TakeColumnChars s)))
      ( Eval
          ( AssertNonEmptyList
              (Eval (Fst (TakeRowFirstChar s)))
              (ParseFailureMsg :<>: RuleRowIndexFirstDigit :<>: Text "The address: " :<>: Text (FromChars s))
              ++ Eval (Fst (TakeRowChars s))
          )
      )
      (TakeRestChars s)

type family FromCharsNatural1 (cs :: [Char]) (res :: Natural) :: Natural where
  FromCharsNatural1 '[] res = res
  FromCharsNatural1 (c : cs) res = FromCharsNatural1 cs (res TL.* 10 + (CharToNat c - CharToNat '0'))

type family FromCharsNatural (cs :: [Char]) :: Natural where
  FromCharsNatural cs = FromCharsNatural1 cs 0

type family CheckedAddressSymbol1 (s :: ([Char], [Char], Constraint)) :: (Symbol, Natural, Constraint) where
  CheckedAddressSymbol1 '(a, b, c) = '(FromChars a, FromCharsNatural b, c)

type family CheckedAddressSymbol (s :: Symbol) :: (Symbol, Natural, Constraint) where
  CheckedAddressSymbol s = CheckedAddressSymbol1 (CheckedAddress2 (ToChars s))

mkAddress' :: forall address column row c. Address' address column row c => (ColumnIndex, RowIndex)
mkAddress' = (unsafeColumnIndexFromString $ symbolVal (Proxy @column), fromIntegral (natVal (Proxy @row)))

type Address' address column row c = (KnownSymbol address, '(column, row, c) ~ CheckedAddressSymbol address, KnownSymbol column, KnownNat row)

type Address address = forall column row c. Address' address column row c => (ColumnIndex, RowIndex)

mkAddress :: forall address. Address address
mkAddress = mkAddress' @address

exampleAddress :: (ColumnIndex, RowIndex)
exampleAddress = mkAddress @"AA123"

-- >>> exampleAddress
-- (ColumnIndex {unColumnIndex = 27},RowIndex {unRowIndex = 123})

-- >>> mkAddress @"A023"
-- Failed to parse the address.
-- The first digit of the row number must be between 1 and 9.
-- The address: A023
-- In the expression: mkAddress @"A023"
-- In an equation for `it_a9Gk': it_a9Gk = mkAddress @"A023"

-- >>> mkAddress @"A1a"
-- Failed to parse the address.
-- The address must be a column index and a row number.
-- The column index must have one or more uppercase letters.
-- The row number must have one or more digits.
-- column:
-- row: 1
-- rest: a
-- In the expression: mkAddress @"A1a"
-- In an equation for `it_a19ZV': it_a19ZV = mkAddress @"A1a"
