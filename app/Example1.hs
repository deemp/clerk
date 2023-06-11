{- FOURMOLU_DISABLE -}
{-
## Example 1

**The goal**: demonstrate formula syntax.

The source code for this example is available [here](app/Example1.hs).

### Extensions
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

{-
### Imports

I import the necessary stuff.
-}

import Clerk ( (.*), (.**), (.+), (./), as, fun, mkRefDefault, funRef,  val, Formula, Ref )
import Clerk.Row ( rowShowDefault )
import Data.Text (Text)
import Clerk.ForExamples

{- LIMA_DISABLE -}

main :: IO ()
main = undefined

{- LIMA_ENABLE -}

{-
### Formulas

Formulas consist of references, functions, and values.

I make references to `Double` values
-}

r1 :: Ref Double
r1 = mkRefDefault @"B4"
r2 :: Ref Double
r2 = mkRefDefault @"E6"
r3 :: Ref Double
r3 = mkRefDefault @"G8"

{-
Next, I convert one of these references to a formula via `funRef` and inspect the formula representation.
-}

t1 :: Text
t1 = showFormula $ funRef r2

-- >>>t1
-- "E6"

{-
Finally, I construct a longer expression and look at its representation.
I convert a literal value to a formula via `val`.
-}

t2 :: Text
t2 = showFormula $ r1 .* r2 .* val 3 .+ r1 .** r2 ./ r3

-- >>>t2
-- "B4*E6*3.0+B4^E6/G8"

{-
Of course, I can mix differently typed references in expressions when necessary.
For this case, I have an unsafe `as` function.
-}

r4 :: Ref Int
r4 = mkRefDefault @"T6"

t3 :: Text
t3 = showFormula $ as @Double (r4 .* r4 .* val 3) .+ r1 .** r2 ./ r3

-- >>>t3
-- "T6*T6*1.5e-323+B4^E6/G8"

{-
This `as` function should not be abused, though. If I need an `Int` instead of a `Double`, I can explicitly use an Excel function.
-}

round_ :: [Formula a] -> Formula Int
round_ = fun "ROUND"

t4 :: Formula Int
t4 = round_ [r1 .** r2 ./ r3]

-- >>>:t t4
-- t4 :: Formula Int

t5 = showFormula t4

-- TODO printing breaks HLS
-- >>> t5
