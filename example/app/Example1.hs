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
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Lens ((&), (+~), (^.))
import Control.Monad (forM, forM_, void, zipWithM)
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

This is our data: numbers that we'd like to make a multiplication table for
-}

numbers :: [Int]
numbers = [1 .. 9]

{-
#### A row with numbers

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/horizontal.png" width = "80%">

`clerk` uses a special `Row` where we can describe a row of data. This monad takes some input, internally converts it into Excel types, and can output something, e.g., a template coordinate. Simultaneously, it will build a template of a row of data. This template will be used when placing the input values onto a sheet. When the values are placed, template coordinates become actual addresses like `A1` or `B1`.

Now, our goal is to construct a `Row` of numbers. We'll need the coordinates of the cells in that row.
-}

mkRow :: [Int] -> Row () [Ref Int]
mkRow ns = forM ns (column blank . const)

{-
#### A column with numbers

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/vertical.png" width = "10%">

To construct a column of numbers, we can use a template for rendering a single column of data. Again, we'll need the coordinate of that cell.
-}

mkCol :: Int -> Row () (Ref Int)
mkCol i = column blank (const i)

{-
#### Table builder

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/table.png" width = "50%">

We use this builder for inner cells. It depends on the coordinates of cells from the column and the row.
-}

mkTable :: Num a => (Ref a, Ref a) -> Row () ()
mkTable (a, b) = column_ blank (const (a .* b))

{-
### Sheet builder

The `Sheet` is used to place `Row`s onto a sheet and glue them together.
Inside `Sheet`, when a `Row` is placed onto a sheet, we can use the
references that it produces in the subsequent expressions.
-}

sheet :: Sheet ()
sheet = do
  let start = coords 2 2
  rowRefs <- place (start & col +~ 2) (mkRow numbers)
  colRefs <- forM numbers $ \n -> place (start & row +~ n + 1) (mkCol n)
  forM_
    [(r, c) | r <- colRefs, c <- rowRefs]
    (\x@(r, c) -> place (coords (r ^. row) (c ^. col)) (mkTable x))

{-
### Result

Finally, we can write the result and get the spreadsheet like the one at the top of this tutorial.
-}

writeWorksheet :: Sheet a -> String -> IO ()
writeWorksheet tb name = do
  ct <- getPOSIXTime
  let xlsx = composeXlsx [(T.pack "List 1", void tb)]
  L.writeFile ("example-" <> name <> ".xlsx") $ X.fromXlsx ct xlsx

main :: IO ()
main = writeWorksheet sheet "1"

{-
To get `example-1.xlsx`, run:

```console
nix develop -c example1
```

With formulas enabled, the sheet looks like this:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/demoFormulas.png" width = "80%">
-}