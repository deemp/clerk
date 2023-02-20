# clerk

`clerk` provides a Haskell eDSL in a library for declarative spreadsheet generation. `clerk` is built on top of the [xlsx](https://hackage.haskell.org/package/xlsx) package and extends upon the [work](https://youtu.be/1xGoa-zEOrQ) of Nickolay Kudasov by making the tables' layout more flexible.

## Features

`clerk` can be used to produce a styled spreadsheet with some data and formulas on it. These formulas are evaluated when the document is loaded into a target spreadsheet system.

The library supports:

- Typed cell references. Example: `CellRef Double`.
- Type-safe arithmetic operations with them. Example: `(a :: CellRef Double) + (b :: CellRef Double)` produces a `CellRef Double`.
- Constructing expressions with given types. Example: `(e :: Expr Double) = "SUM" |$| [a |:| b]`, `e` translates to `SUM(A1:B1)` (actual value depends on the values of `a` and `b`).
- Conditional styles, formatting, column widths.

The example below demonstrates most of these features.

<!-- FOURMOLU_DISABLE -->

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

```haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
```
<!-- LIMA_DISABLE
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
LIMA_ENABLE -->
<!-- FOURMOLU_ENABLE -->

### Imports

And import the necessary stuff.

```haskell
import Clerk
import Codec.Xlsx qualified as X
import Control.Lens ((&), (+~), (^.))
import Control.Monad (forM, forM_, void, zipWithM)
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
```

### Tables

The tables that we'd like to construct are:

- A row with numbers
- A column with numbers
- A table with results of multiplication of the numbers from these row and column

#### Data

This is our data: numbers that we'd like to make a multiplication table for

```haskell
numbers :: [Int]
numbers = [1 .. 9]
```

#### A row with numbers

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/horizontal.png" width = "80%">

`clerk` uses a special `Row` where we can describe a row of data. This monad takes some input, internally converts it into Excel types, and can output something, e.g., a template coordinate. Simultaneously, it will build a template of a row of data. This template will be used when placing the input values onto a sheet. When the values are placed, template coordinates become actual addresses like `A1` or `B1`.

Now, our goal is to construct a `Row` of numbers. We'll need the coordinates of the cells in that row.

```haskell
mkRow :: [Int] -> Row () [Ref Int]
mkRow ns = forM ns (column blank . const)
```

#### A column with numbers

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/vertical.png" width = "10%">

To construct a column of numbers, we can use a template for rendering a single column of data. Again, we'll need the coordinate of that cell.

```haskell
mkCol :: Int -> Row () (Ref Int)
mkCol i = column blank (const i)
```

#### Table builder

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/table.png" width = "50%">

We use this builder for inner cells. It depends on the coordinates of cells from the column and the row.

```haskell
mkTable :: Num a => (Ref a, Ref a) -> Row () ()
mkTable (a, b) = column_ blank (const (a .* b))
```

### Sheet builder

The `Sheet` is used to place `Row`s onto a sheet and glue them together.
Inside `Sheet`, when a `Row` is placed onto a sheet, we can use the
references that it produces in the subsequent expressions.

```haskell
sheet :: Sheet ()
sheet = do
  let start = coords 2 2
  rowRefs <- place (start & col +~ 2) (mkRow numbers)
  colRefs <- forM numbers $ \n -> place (start & row +~ n + 1) (mkCol n)
  forM_
    [(r, c) | r <- colRefs, c <- rowRefs]
    (\x@(r, c) -> place (coords (r ^. row) (c ^. col)) (mkTable x))
```

### Result

Finally, we can write the result and get the spreadsheet like the one at the top of this tutorial.

```haskell
writeWorksheet :: Sheet a -> String -> IO ()
writeWorksheet tb name = do
  ct <- getPOSIXTime
  let xlsx = composeXlsx [(T.pack "List 1", void tb)]
  L.writeFile ("example-" <> name <> ".xlsx") $ X.fromXlsx ct xlsx

main :: IO ()
main = writeWorksheet sheet "1"
```

To get `example-1.xlsx`, run:

```console
nix develop -c example1
```

With formulas enabled, the sheet looks like this:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example1/demoFormulas.png" width = "80%">

<!-- FOURMOLU_DISABLE -->

## Example 2

**The goal**: describe and generate a spreadsheet that calculates the pressure data given some volume data and constants.

The source code for this example is available in the [Example2.hs](example/app/Example2.hs).
The program produces an `xlsx` file that looks as follows:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/demoValues.png" width = "80%">

Alternatively, with formulas enabled:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/demoFormulas.png" width = "80%">

The below sections describe how such a spreadsheet can be constructed.

### Extensions

We'll need several language extensions.

```haskell
-- to access the fields of records like a.b
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
```

<!-- FOURMOLU_ENABLE -->

### Imports

And import the necessary stuff.

```haskell
import Clerk
import Codec.Xlsx qualified as X
import Codec.Xlsx.Formatted qualified as X
import Control.Lens ((%~), (&), (+~), (?~))
import Control.Monad (void)
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
```

### Tables

The tables that we'd like to construct are:

- A table per a constant's value (three of them)
- A volume & pressure table
- A constants' header
- A volume & pressure header

#### Constants' values

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/constants.png" width = "50%">

In our case, each constant has the same type of the numeric value - `Double`.
However, it might be the case that in another set of constants, they'll have different types.
That's why, in our case, we'll construct a table with a single row per a constant and later stack the constants' tables together.
We can keep a constant's data in a record.

```haskell
data ConstantData a = ConstantData
  { constantName :: String
  , constantSymbol :: String
  , constantValue :: a
  , constantUnits :: String
  }
```

Next, we can group the constants.

```haskell
data Constants f = Constants
  { gasConstant :: f Double
  , numberOfMoles :: f Double
  , temperature :: f Double
  }

type ConstantsInput = Constants ConstantData
```

At last, here's our constants' data.

```haskell
constants :: ConstantsInput
constants =
  Constants
    { gasConstant = ConstantData "GAS CONSTANT" "R" 0.08206 "L.atm/mol.K"
    , numberOfMoles = ConstantData "NUMBER OF MOLES" "n" 1 "moles"
    , temperature = ConstantData "TEMPERATURE(K)" "T" 273.2 "K"
    }
```

Furthermore, we'd like to style the constants' tables, so let's prepare the styles. We'll reuse these styles in other tables.

```haskell
data Colors = LightBlue | LightGreen | Blue | Green
instance ToARGB Colors where
  toARGB :: Colors -> String
  toARGB = \case
    LightBlue -> "90CCFFFF"
    LightGreen -> "90CCFFCC"
    Blue -> "FF99CCFF"
    Green -> "FF00FF00"

blue :: FormatCell
blue = mkColor Blue

lightBlue :: FormatCell
lightBlue = mkColor LightBlue

green :: FormatCell
green = mkColor Green

mixed :: FormatCell
mixed coords_ idx = mkColor (if even idx then LightGreen else LightBlue) coords_ idx
```

Additionally, we compose a transformation of a `FormatCell` for the number format

```haskell
use2decimalDigits :: FCTransform
use2decimalDigits fcTransform =
  fcTransform & X.formattedFormat %~ (\format -> format & X.formatNumberFormat ?~ X.StdNumberFormat X.Nf2Decimal)
```

And a transform for centering the cell contents

```haskell
alignCenter :: FCTransform
alignCenter = horizontalAlignment X.CellHorizontalAlignmentCenter
```

Now, we can make a `Row` for a constant.
We'll later use this builder for each constant separately.

We get a pair of outputs:

- Top left cell of a constant's table. That is, the cell with that constant's name.
- The value of the constant.

Later, the outputs of this and other `Row`s will be used to relate the positions of tables on a sheet.

```haskell
constantBuilder :: ToCellData a => Row (ConstantData a) (Ref (), Ref a)
constantBuilder = do
  refTopLeft <- column lightBlue constantName
  column_ lightBlue constantSymbol
  refValue <- column (lightBlue .& use2decimalDigits) constantValue
  column_ lightBlue constantUnits
  return (refTopLeft, refValue)
```

#### Volume & Pressure values

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/valuesFormulas.png" width = "50%">

To fill this table, we'll take the some data and combine it with the constants.

```haskell
newtype Volume = Volume {volume :: Double}

volumeData :: [Volume]
volumeData = take 10 $ Volume <$> [1 ..]
```

To pass the constants' references in a structured way, we make a helper type.

```haskell
data ConstantsRefs = ConstantsRefs
  { refGas :: Ref Double
  , refNumberOfMoles :: Ref Double
  , refTemperature :: Ref Double
  }
```

Next, we define a function to produce a builder for volume and pressure. We pass references to constants' values to this builder

```haskell
valuesBuilder :: ConstantsRefs -> Row Volume ()
valuesBuilder ConstantsRefs{..} = do
  refVolume <- column mixed volume
  let pressure' = refGas .* refNumberOfMoles .* refTemperature ./ refVolume
  column_ (mixed .& use2decimalDigits) (const pressure')
```

#### Constants' header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/constantsHeader.png" width = "50%">

We won't use records here. Instead, we'll put the names of the columns straight into the `Row`.

The outputs will be the coordinates of the top left cell and the top right cell of this table.

```haskell
constantsHeaderBuilder :: Row () (Ref (), Ref ())
constantsHeaderBuilder = do
  refTopLeft <- columnWidth 20 (blue .& alignCenter) (const "constant")
  columnWidth_ 8 (blue .& alignCenter) (const "symbol")
  column_ (blue .& alignCenter) (const "value")
  refTopRight <- columnWidth 13 (blue .& alignCenter) (const "units")
  return (refTopLeft, refTopRight)
```

#### Volume & Pressure header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/valuesHeader.png" width = "50%">

For this header, we'll also put the names of columns straight inside the builder.

```haskell
valuesHeaderBuilder :: Row () Coords
valuesHeaderBuilder = do
  tl <- columnWidth 12 green (const "VOLUME (L)")
  columnWidth_ 16 green (const "PRESSURE (atm)")
  return (toCoords tl)
```

### Sheet builder

The `SheetBuilder` is used to place `Row`s onto a sheet and glue them together.
Inside `SheetBuilder`, when a `Row` is placed onto a sheet, we can use the
references that it produces in the subsequent expressions.

```haskell
sheet :: Sheet ()
sheet = do
  (constantsHeaderTL, constantsHeaderTR) <- place (coords 2 2) constantsHeaderBuilder
  (gasTL, gas) <- place1 (constantsHeaderTL & row +~ 2) constants.gasConstant constantBuilder
  (nMolesTL, nMoles) <- place1 (gasTL & row +~ 1) constants.numberOfMoles constantBuilder
  temperature <- snd <$> place1 (nMolesTL & row +~ 1) constants.temperature constantBuilder
  valuesHeaderTL <- place (constantsHeaderTR & row +~ 2) valuesHeaderBuilder
  placeN (valuesHeaderTL & row +~ 2) volumeData (valuesBuilder $ ConstantsRefs gas nMoles temperature)
```

### Result

Finally, we can write the result and get the spreadsheet like the one at the top of this tutorial.

```haskell
writeWorksheet :: Sheet a -> String -> IO ()
writeWorksheet tb name = do
  ct <- getPOSIXTime
  let xlsx = composeXlsx [(T.pack "List 1", void tb)]
  L.writeFile ("example-" <> name <> ".xlsx") $ X.fromXlsx ct xlsx

main :: IO ()
main = writeWorksheet sheet "2"
```

To get `example-2.xlsx`, run:

```console
nix develop -c example2
```

With formulas enabled, the sheet looks like this:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example2/demoFormulas.png" width = "80%">

## Contribute

This project provides a dev environment via a `Nix` flake.

1. With [flakes enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes), run:

    ```console
    nix develop
    cabal build
    ```

1. This `README.md` is generated from several files. If you edit them, re-generate it.

    ```console
    cabal test docs
    ```

1. (Optionally) Start `VSCodium` with `Haskell` extensions.

    1. Write settings and run `VSCodium`.

        ```console
        nix run .#writeSettings
        nix run .#codium .
        ```

    1. Open a `Haskell` file. `Haskell Language Server` should soon start giving you hints.

1. Study these links if you'd like to learn more about the tools used in this flake:

    - [Prerequisites](https://github.com/deemp/flakes#prerequisites)
    - `Haskell` project [template](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme)
    - [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)
