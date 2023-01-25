{-
## Example

The goal: describe and generate a spreadsheet that calculates the pressure data given some volume data and constants.

The source code for this example is available in the [example](./example) directory.
The program produces an `xlsx` file that looks as follows:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/demoValues.png" width = "80%">

Alternatively, with formulas enabled:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/demoFormulas.png" width = "80%">

The below sections describe how such a spreadsheet can be constructed.

### Extensions

We'll need several language extensions.
-}
{- FOURMOLU_DISABLE -}
{-# LANGUAGE OverloadedRecordDot #-} -- access the fields of records like a.b
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{- FOURMOLU_ENABLE -}

{-
### Imports

And import the necessary stuff.
-}

module Main (main) where

import Clerk
import Codec.Xlsx qualified as X
import Codec.Xlsx.Formatted qualified as X
import Control.Lens ((%~), (&), (?~))
import Control.Monad (void)
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)

{-
### Tables

The tables that we'd like to construct are:

- A table per a constant's value (three of them)
- Volume & pressure table
- Constants' header
- Volume & pressure header

#### Constants' values

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/constants.png" width = "50%">

In our case, each constant has the same type of the numeric value - `Double`.
However, it might be the case that in another set of constants, they'll have different types.
That's why, in our case, we'll construct a table with a single row per a constant and later stack the constants' tables together.
We can keep a constant's data in a record.
-}

data ConstantData a = ConstantData
  { constantName :: String
  , constantSymbol :: String
  , constantValue :: a
  , constantUnits :: String
  }

{-
Next, we can group the constants.
-}

data Constants f = Constants
  { gasConstant :: f Double
  , numberOfMoles :: f Double
  , temperature :: f Double
  }

type ConstantsInput = Constants ConstantData

constants :: ConstantsInput
constants =
  Constants
    { gasConstant = ConstantData "GAS CONSTANT" "R" 0.08206 "L.atm/mol.K"
    , numberOfMoles = ConstantData "NUMBER OF MOLES" "n" 1 "moles"
    , temperature = ConstantData "TEMPERATURE(K)" "T" 273.2 "K"
    }

{-
Furthermore, we'd like to style the constants' tables, so let's prepare the styles. We'll reuse these styles in other tables.
-}

data Colors = LightBlue | LightGreen | Blue | Green
instance Show Colors where
  show :: Colors -> String
  show = \case
    LightBlue -> "90CCFFFF"
    LightGreen -> "90CCFFCC"
    Blue -> "FF99CCFF"
    Green -> "FF00FF00"

blue :: FormatCell
blue = mkColorStyle Blue

lightBlue :: FormatCell
lightBlue = mkColorStyle LightBlue

green :: FormatCell
green = mkColorStyle Green

mixed :: FormatCell
mixed coords idx = mkColorStyle (if even idx then LightGreen else LightBlue) coords idx

{-
Additionally, we compose a transformation of a `FormatCell` for the number format
-}

use2decimalDigits :: FCTransform
use2decimalDigits fcTransform =
  fcTransform & X.formattedFormat %~ (\format -> format & X.formatNumberFormat ?~ X.StdNumberFormat X.Nf2Decimal)

{-
And a transform for centering the cell contents
-}
alignCenter :: FCTransform
alignCenter = horizontalAlignment X.CellHorizontalAlignmentCenter

{-
Now, we can make a `RowBuilder` for a constant.
We'll later use this builder for each constant separately.

We get a pair of outputs:

- Top left cell of a constant's table. That is, the cell with that constant's name.
- The value of the constant.

Later, the outputs of this and other `RowBuilder`s will be used to relate the positions of tables on a sheet.
-}

constantBuilder :: ToCellData a => RowBuilder (ConstantData a) CellData (Coords, CellRef a)
constantBuilder = do
  refTopLeft <- column lightBlue constantName
  column_ lightBlue constantSymbol
  refValue <- column (lightBlue +> use2decimalDigits) constantValue
  column_ lightBlue constantUnits
  return (unCell refTopLeft, refValue)

{-
#### Volume & Pressure values

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/valuesFormulas.png" width = "50%">

To fill this table, we'll take the some data and combine it with the constants.
-}

newtype Volume = Volume {volume :: Double}

volumeData :: [Volume]
volumeData = take 10 $ Volume <$> [1 ..]

{-
To pass the constants' references in a structured way, we make a helper type.
-}

data ConstantsRefs = ConstantsRefs
  { refGas :: CellRef Double
  , refNumberOfMoles :: CellRef Double
  , refTemperature :: CellRef Double
  }

{-
Next, we define a function to produce a builder for volume and pressure.
-}

valuesBuilder :: ConstantsRefs -> RowBuilder Volume CellData ()
valuesBuilder ConstantsRefs{..} = do
  refVolume <- column mixed volume
  let pressure' = refGas |*| refNumberOfMoles |*| refTemperature |/| refVolume
  column_ (mixed +> use2decimalDigits) (const pressure')

{-
#### Constants' header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/constantsHeader.png" width = "50%">

We won't use records here. Instead, we'll put the names of the columns straight into the `RowBuilder`.

The outputs will be the coordinates of the top left cell and the top right cell of this table.
-}

constantsHeaderBuilder :: RowBuilder () CellData (Coords, Coords)
constantsHeaderBuilder = do
  refTopLeft <- columnWidth 20 (blue +> alignCenter) (const "constant")
  columnWidth_ 8 (blue +> alignCenter) (const "symbol")
  column_ (blue +> alignCenter) (const "value")
  refTopRight <- columnWidth 13 (blue +> alignCenter) (const "units")
  return (unCell refTopLeft, unCell refTopRight)

{-
#### Volume & Pressure header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/valuesHeader.png" width = "50%">

For this header, we'll also put the names of columns straight inside the builder.
-}

valuesHeaderBuilder :: RowBuilder () CellData Coords
valuesHeaderBuilder = do
  tl <- columnWidth 12 green (const "VOLUME (L)")
  columnWidth_ 16 green (const "PRESSURE (atm)")
  return (unCell tl)

{-
### Sheet builder

The `SheetBuilder` is used to place `RowBuilder`s onto a sheet and glue them together.
Inside `SheetBuilder`, when a `RowBuilder` is placed onto a sheet, we can use the
references that it produces in the subsequent expressions.
-}

full :: SheetBuilder ()
full = do
  (constantsHeaderTL, constantsHeaderTR) <- placeInput (Coords 2 2) () constantsHeaderBuilder
  (gasTL, gas) <- placeInput (overRow (+ 2) constantsHeaderTL) constants.gasConstant constantBuilder
  (nMolesTL, nMoles) <- placeInput (overRow (+ 1) gasTL) constants.numberOfMoles constantBuilder
  temperature <- snd <$> placeInput (overRow (+ 1) nMolesTL) constants.temperature constantBuilder
  valuesHeaderTL <- placeInput (overCol (+ 2) constantsHeaderTR) () valuesHeaderBuilder
  placeInputs_ (overRow (+ 2) valuesHeaderTL) volumeData (valuesBuilder $ ConstantsRefs gas nMoles temperature)

{-
### Result

Finally, we can write the result and get the spreadsheet like the one that you've seen at the top of this tutorial.
-}

writeWorksheet :: SheetBuilder a -> String -> IO ()
writeWorksheet tb name = do
  ct <- getPOSIXTime
  let xlsx = composeXlsx [(T.pack "List 1", void tb)]
  L.writeFile ("example-" <> name <> ".xlsx") $ X.fromXlsx ct xlsx

writeEx :: IO ()
writeEx = writeWorksheet full "1"

main :: IO ()
main = writeEx

{-
Run

```console
cd example
nix develop
cabal run
```

to get `example/example-1.xlsx`.

With formulas enabled, `example-1.xlsx` looks like this:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/demoFormulas.png" width = "80%">
-}