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

{-
### Imports

We import the necessary stuff.
-}

import Clerk ( (.*), (.**), (.+), (./), as, fun, mkRef, ref, showFormula, val, Formula, Ref )
import Data.Text (Text)

{- LIMA_DISABLE -}

main :: IO ()
main = undefined

{- LIMA_ENABLE -}

{-
### Formulas

Formulas consist of references, functions, and values.

We define a couple of helper functions just for this example.
These function simplify working with references and formulas.

Now, we pretend that there are values with given types and that we can get references to them.

First, we make a couple of references to `Int` values.
-}

r1, r2, r3 :: Ref Double
r1 = mkRef "B4"
r2 = mkRef "E6"
r3 = mkRef "G8"

{-
Next, we convert one of these references to a formula via `ref` and inspect its representation.
-}

t1 :: Text
t1 = showFormula $ ref r2

-- >>>t1
-- "E6"

{-
Finally, we construct a longer expression and look at its representation.
We convert a literal value to a formula via `val`.
-}

t2 :: Text
t2 = showFormula $ r1 .* r2 .* val 3 .+ r1 .** r2 ./ r3

-- >>>t2
-- "B4*E6*3.0+B4^E6/G8"

{-
Of course, we can mix differently typed references in expressions when necessary.
For this case, we have an unsafe `as` function.
-}

r4 :: Ref Int
r4 = mkRef "T6"

t3 :: Text
t3 = showFormula $ as @Double (r4 .* r4 .* val 3) .+ r1 .** r2 ./ r3

-- >>>t3
-- "T6*T6*1.5e-323+B4^E6/G8"

{-
This `as` function should not be abused, though. If we need an `Int` instead of a `Double`, we can explicitly use an Excel function.
-}

round_ :: [Formula a] -> Formula Int
round_ = fun "ROUND"

t4 :: Formula Int
t4 = round_ [r1 .** r2 ./ r3]

-- >>>:t t4
-- t4 :: Formula Int
