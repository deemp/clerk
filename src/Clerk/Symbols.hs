module Clerk.Symbols where

import GHC.TypeLits

type family FromChars1 (cs :: [Char]) (res :: Symbol) :: Symbol where
  FromChars1 '[] res = res
  FromChars1 (c : cs) res = FromChars1 cs (ConsSymbol c res)

type family FromChars (cs :: [Char]) :: Symbol where
  FromChars cs = FromChars1 (Reverse cs '[]) ""

type family Reverse (s :: [Char]) (res :: [Char]) :: [Char] where
  Reverse '[] res = res
  Reverse (c : cs) res = Reverse cs (c : res)

type family ToChars1 (s :: Maybe (Char, Symbol)) (r :: [Char]) :: [Char] where
  ToChars1 Nothing s = Reverse s '[]
  ToChars1 ('Just '(c, cs)) s = ToChars1 (UnconsSymbol cs) (c ': s)

type family ToChars (s :: Symbol) :: [Char] where
  ToChars s = ToChars1 (UnconsSymbol s) '[]

type family CharBetween1 (c1 :: Ordering) (c2 :: Ordering) :: Bool where
  CharBetween1 EQ LT = True
  CharBetween1 LT EQ = True
  CharBetween1 LT LT = True
  CharBetween1 a b = False

type family CharBetween (c :: Char) (lowerBound :: Char) (upperBound :: Char) :: Bool where
  CharBetween c lowerBound upperBound = CharBetween1 (CmpChar lowerBound c) (CmpChar c upperBound)

type family AssertNonEmptyList (s :: [a]) (errorMessage :: ErrorMessage) :: [a] where
  AssertNonEmptyList '[] errorMessage = TypeError errorMessage
  AssertNonEmptyList a errorMessage = a