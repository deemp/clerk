{- FOURMOLU_DISABLE -}
{-
## Example 1

**The goal**: demonstrate formula syntax.

The source code for this example is available [here](app/Example1.hs).
-}
{- LIMA_DISABLE -}
-- {-# HLINT ignore "Redundant bracket" #-}
{- FOURMOLU_ENABLE -}
{- LIMA_ENABLE -}

{-
### Imports

We import the necessary stuff.
-}

import Clerk
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
-}

{-
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
