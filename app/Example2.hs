{- FOURMOLU_DISABLE -}
{-
## Example 2

**The goal**: describe and generate a spreadsheet with a simple multiplication table.

The source code for this example is available [here](app/Example2.hs).

The program produces an `xlsx` file that looks as follows:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/demoValues.png" width = "80%">

With formulas enabled:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/demoFormulas.png" width = "80%">

The below sections describe how such a spreadsheet can be constructed.

### Extensions

We'll need several language extensions.
-}
{-# LANGUAGE ImportQualifiedPost #-}
{- LIMA_DISABLE -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{- LIMA_ENABLE -}
{- FOURMOLU_ENABLE -}

{-
### Imports

We import the necessary stuff.
-}

import Clerk
import Control.Monad (forM, forM_, void)
import Data.Text qualified as T
import Lens.Micro ((&), (+~), (^.))

{-
### Tables

The tables that we'd like to construct are:

- A vertical header
- A horizontal header
- A table with results of multiplication of the numbers from these headers

#### A vertical header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/vertical.png" width = "10%">

`clerk` provides the `RowI` monad.
This monad takes some `i`nput, internally converts it into Excel types, and outputs something, e.g., a cell reference.
In background, it writes a template of a horizontal block of cells - a **row**.
This row is used for placing the input values onto a sheet.

A vertical block of cells can be represented as several horizontal blocks of cells placed under each other.
So, as a template, we use a `RowI` with one integer as an input.

As we don't need any formatting, we use `blank` cells for templates.

We place the rows for each input value and collect the references.
Each row is shifted relative to the input coordinates.
-}

mkVertical :: Coords -> [Int] -> Sheet [Ref Int]
mkVertical coords numbers =
  forM (zip [0 ..] numbers) $ \(idx, number) ->
    place1
      (coords & row +~ idx + 2)
      number
      ((columnRef blank (const number)) :: RowI Int (Ref Int))

{-
#### A horizontal header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/horizontal.png" width = "80%">

For a horizontal header, we make a row of numbers and collect the references to all its cells.
As we don't care about the type of inputs, we use the `Row` type.

In the `Sheet` monad, we place this row starting at a specified coordinate.
-}

mkHorizontal :: Coords -> [Int] -> Sheet [Ref Int]
mkHorizontal coords numbers =
  place
    (coords & col +~ 2)
    ((forM numbers $ \n -> columnRef blank (const n)) :: Row [Ref Int])

{-
#### Table builder

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/table.png" width = "50%">

For inner cells, we use single-cell rows for each input.
As we don't need any info about these cells, we use the `Row ()` type.
-}

mkTable :: [(Ref Int, Ref Int)] -> Sheet ()
mkTable cs =
  forM_ cs $ \(r, c) -> do
    coords <- mkCoords (c ^. col) (r ^. row)
    place coords ((column blank (const (r .* c))) :: Row ())

{-
### Sheet

Now, we combine all functions.
-}

sheet :: Sheet ()
sheet = do
  start <- mkCoords 2 2
  let numbers = [1 .. 9]
  cs <- mkHorizontal start numbers
  rs <- mkVertical start numbers
  mkTable [(r, c) | r <- rs, c <- cs]

{-
### Result

Finally, we can write the result and get a spreadsheet like the one at the beginning of [Example 2](#example-2).
-}

main :: IO ()
main = writeXlsx "example2.xlsx" [(T.pack "List 1", void sheet)]

{-
To get `./example2.xlsx`, run:

```console
nix run .#example2
-- or
cabal run example2
```

With formulas enabled, the sheet looks like this:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/demoFormulas.png" width = "80%">
-}