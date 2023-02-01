{- FOURMOLU_DISABLE -}
{-
## Example 1

**The goal**: describe and generate a spreadsheet with a simple multiplication table.

The source code for this example is available in the [Example1.hs](example/app/Example1.hs).
The program produces an `xlsx` file that looks as follows:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/demoValues.png" width = "80%">

Alternatively, with formulas enabled:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/demoFormulas.png" width = "80%">

The below sections describe how such a spreadsheet can be constructed.

### Extensions

We'll need several language extensions.
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- LIMA_DISABLE -}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{- LIMA_ENABLE -}
{- FOURMOLU_ENABLE -}

{-
### Imports

And import the necessary stuff.
-}

import Clerk
import Codec.Xlsx qualified as X
import Control.Monad (forM_, void, zipWithM)
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)

{-
### Tables

The tables that we'd like to construct are:

- A row with numbers
- A column with numbers
- A table with results of multiplication of the numbers from these row and column

#### Data

This is our data:

- numbers that we'd like to make a multiplication table for
- indices that we'd like to use when placing the values
-}

numbers :: [Int]
numbers = [1 .. 9]

indices :: [Int]
indices = [0, 1 .. 8]

{-
#### Row and column builder

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/horizontal.png" width = "80%">

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/vertical.png" width = "10%">

We use this builder for boundary cells.
-}

boundaryBuilder :: Int -> RowBuilder' () (CellRef Int)
boundaryBuilder i = column blank (const i)

{-
#### Table builder

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/table.png" width = "50%">

We use this builder for inner cells. It depends on the coordinates of cells from the column and the row.
-}

tableBuilder :: Num a => (CellRef a, CellRef a) -> RowBuilder' () ()
tableBuilder (a, b) = column_ blank (const (a |*| b))

{-
### Sheet builder

The `SheetBuilder` is used to place `RowBuilder'`s onto a sheet and glue them together.
Inside `SheetBuilder`, when a `RowBuilder'` is placed onto a sheet, we can use the
references that it produces in the subsequent expressions.
-}

sheet :: SheetBuilder ()
sheet = do
  let tl = coords 2 2
  hs <- zipWithM (\i n -> placeInput (overCol (+ (2 + i)) tl) () (boundaryBuilder n)) indices numbers
  vs <- zipWithM (\i n -> placeInput (overRow (+ (2 + i)) tl) () (boundaryBuilder n)) indices numbers
  forM_ (do r <- vs; c <- hs; pure (r, c)) (\x@(r, c) -> placeInput (coords (getRow r) (getCol c)) () (tableBuilder x))

{-
### Result

Finally, we can write the result and get the spreadsheet like the one that at the top of this tutorial.
-}

writeWorksheet :: SheetBuilder a -> String -> IO ()
writeWorksheet tb name = do
  ct <- getPOSIXTime
  let xlsx = composeXlsx [(T.pack "List 1", void tb)]
  L.writeFile ("example-" <> name <> ".xlsx") $ X.fromXlsx ct xlsx

main :: IO ()
main = writeWorksheet sheet "1"

{-
To get `example/example-1.xlsx`, run:

```console
cd example && nix develop -c cabal run example-1
```

With formulas enabled, the sheet looks like this:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/demoFormulas.png" width = "80%">
-}