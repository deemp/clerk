{- FOURMOLU_DISABLE -}
{-
## Example 1

**The goal**: demonstrate formula syntax.

The source code for this example is available [here](app/Example1.hs).
-}
{- LIMA_DISABLE -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{- FOURMOLU_ENABLE -}
{- LIMA_ENABLE -}

{-
### Imports

We import the necessary stuff.
-}
import Clerk
import Data.Text (Text)
import ForExamples (mkRef, showFormula)

{- LIMA_DISABLE -}
main :: IO ()
main = undefined

{- LIMA_ENABLE -}

{-
### Formulas

Formulas consist of references, functions, and values.

We pretend that there are values with given types and that we can get references to them.

First, we make a couple of references to `Int` values.
-}

r1 :: Ref Int
r1 = mkRef 2 4

r2 :: Ref Int
r2 = mkRef 5 6

r3 :: Ref Double
r3 = mkRef 7 8

t1 :: Text
t1 = showFormula $ toFormula r2

-- >>>t1
-- "E6"

t2 :: Text
t2 = showFormula $ (r1 .* r2) .* (toFormula @Int 3) .+ r1 .^ r2 ./ (unsafeChangeType r3)

-- >>>t2
-- "B4*E6+B4^E6/G8"
