{- FOURMOLU_DISABLE -}
{-
## Example 3

**The goal**: describe and generate a spreadsheet that calculates the pressure data given some volume data and constants.

The source code for this example is available [here](app/Example3.hs).

The program produces an `xlsx` file that looks as follows:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/demoValues.png" width = "80%">

With formulas enabled:l

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/demoFormulas.png" width = "80%">

The below sections describe how such a spreadsheet can be constructed.

### Extensions

We'll need several language extensions.
-}

-- to access the fields of records like a.b
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- FOURMOLU_ENABLE -}

{-
### Imports

And import the necessary stuff.
-}

import Clerk
import Codec.Xlsx qualified as X
import Codec.Xlsx.Formatted qualified as X
import Data.Text qualified as T
import Lens.Micro ((%~), (&), (+~), (?~))

{-
### Tables

The tables that we'd like to construct are:

- A table per a constant's value (three of them)
- A volume and pressure table
- A constants' header
- A volume and pressure header

#### constants values

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/constants.png" width = "50%">

In our case, each constant has the same type of the numeric value - `Double`.
However, it might be the case that in another set of constants, they'll have different types.
That's why, we'll construct a table with a single row per a constant and later place the constants' tables under each other.
We'll store constant data in a record.
-}

data ConstantData a = ConstantData
  { constantName :: String
  , constantSymbol :: String
  , constantValue :: a
  , constantUnits :: String
  }

{-
Next, we group the constants.
-}

data Constants f = Constants
  { gasConstant :: f Double
  , numberOfMoles :: f Double
  , temperature :: f Double
  }

type ConstantsInput = Constants ConstantData

{-
Following that, we record the constants data.
-}

constants :: ConstantsInput
constants =
  Constants
    { gasConstant = ConstantData "GAS CONSTANT" "R" 0.08206 "L.atm/mol.K"
    , numberOfMoles = ConstantData "NUMBER OF MOLES" "n" 1 "moles"
    , temperature = ConstantData "TEMPERATURE(K)" "T" 273.2 "K"
    }

{-
Now, we can make a `RowI` for a constant input.
We use a `RowI` because this row cares about the `i`nput type.
We'll later use this row for each constant separately.

We get a pair of outputs:

- Top left cell of a constant's table. That is, the cell with that constant's name.
- The value of the constant.

Later, we'll use these outputs to relate the positions of tables on a sheet.

Notice that we use styles like `lightBlue` here. These styles are defined in the [Styles](#styles) section.
-}

constant :: ToCellData a => RowI (ConstantData a) (Ref (), Ref a)
constant = do
  refTopLeft <- columnRef lightBlue constantName
  column lightBlue constantSymbol
  refValue <- columnRef (lightBlue .& with2decimalDigits) constantValue
  column lightBlue constantUnits
  return (refTopLeft, refValue)

{-
#### volume and pressure values

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/valuesFormulas.png" width = "50%">

To fill this table, we'll take some data and combine it with the constants.
-}

newtype Volume = Volume {volume :: Double}

volumeData :: [Volume]
volumeData = Volume <$> [1 .. 10]

{-
To pass the constants references in a structured way, we make a helper type.
-}

data ConstantsRefs = ConstantsRefs
  { refGasConstant :: Ref Double
  , refNumberOfMoles :: Ref Double
  , refTemperature :: Ref Double
  }

{-
Next, we define a function to produce a row for volume and pressure.
-}

values :: ConstantsRefs -> RowI Volume ()
values ConstantsRefs{..} = do
  refVolume <- columnRef alternatingColors volume
  let pressure' = refGasConstant .* refNumberOfMoles .* refTemperature ./ refVolume
  column (alternatingColors .& with2decimalDigits) (const pressure')

{-
#### Constants' header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/constantsHeader.png" width = "50%">

We won't use records here. Instead, we'll put the names of the columns straight into the `Row`.

The outputs will be the coordinates of the top left cell and the top right cell of this table.
-}

constantsHeader :: Row (Ref (), Ref ())
constantsHeader = do
  let style :: FormatCell
      style = blue .& alignedCenter
  refTopLeft <- columnWidthRef 20 style (const "constant")
  columnWidth 8 style (const "symbol")
  column style (const "value")
  refTopRight <- columnWidthRef 13 style (const "units")
  return (refTopLeft, refTopRight)

{-
#### Volume & Pressure header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/valuesHeader.png" width = "50%">

For this header, we'll also put the names of columns straight into a row.
-}

valuesHeader :: Row (Ref ())
valuesHeader = do
  refTopLeft <- columnWidthRef 12 green (const "VOLUME (L)")
  columnWidth 16 green (const "PRESSURE (atm)")
  return refTopLeft

{-
### Sheet builder

At last, we combine all rows.
-}

sheet :: Sheet ()
sheet = do
  start <- mkCoords 2 2
  (constantsHeaderTL, constantsHeaderTR) <- place start constantsHeader
  (gasTL, gas) <- place1 (constantsHeaderTL & row +~ 2) constants.gasConstant constant
  (nMolesTL, nMoles) <- place1 (gasTL & row +~ 1) constants.numberOfMoles constant
  temperature <- snd <$> place1 (nMolesTL & row +~ 1) constants.temperature constant
  valuesHeaderTL <- place (constantsHeaderTR & col +~ 2) valuesHeader
  placeN (valuesHeaderTL & row +~ 2) volumeData (values $ ConstantsRefs gas nMoles temperature)

{-
### Styles

We used several styles to format the tables. This is how these styles are defined.
-}

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

alternatingColors :: FormatCell
alternatingColors coords index = mkColor (if even index then LightGreen else LightBlue) coords index

{-
Additionally, we compose an `FCTransform` for the number format.
Such a transform is used to accumulate cell formatting.
-}

with2decimalDigits :: FCTransform
with2decimalDigits fcTransform =
  fcTransform & X.formattedFormat %~ X.formatNumberFormat ?~ X.StdNumberFormat X.Nf2Decimal

{-
And we make a transform for centering the cell contents.
-}
alignedCenter :: FCTransform
alignedCenter = horizontalAlignment X.CellHorizontalAlignmentCenter

{-
### Result

Finally, we write the result and get the spreadsheet like the one at the beginning of [Example 3](#example-3).
-}

main :: IO ()
main = writeXlsx "example3.xlsx" [(T.pack "List 1", sheet)]

{-
To get `./example3.xlsx`, run:

```console
nix run .#example3
-- or
cabal run example3
```

With formulas enabled, the sheet looks like this:

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/demoFormulas.png" width = "80%">
-}