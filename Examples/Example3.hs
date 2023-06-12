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

I'll need several language extensions.
-}

-- to access the fields of records like a.b
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
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

The tables that I'd like to construct are:

- A table per a constant's value (three of them)
- A volume and pressure table
- A constants' header
- A volume and pressure header

#### constants values

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/constants.png" width = "50%">

In our case, each constant has the same type of the numeric value - `Double`.
However, it might be the case that in another set of constants, they'll have different types.
That's why, I'll construct a table with a single row per a constant and later place the constants' tables under each other.
I'll store constant data in a record.
-}

data ConstantData a = ConstantData
  { constantName :: String
  , constantSymbol :: String
  , constantValue :: a
  , constantUnits :: String
  }

{-
Next, I group the constants.
-}

data Constants f = Constants
  { gasConstant :: f Double
  , numberOfMoles :: f Double
  , temperature :: f Double
  }

type ConstantsInput = Constants ConstantData

{-
Following that, I record the constants data.
-}

constants :: ConstantsInput
constants =
  Constants
    { gasConstant = ConstantData "GAS CONSTANT" "R" 0.08206 "L.atm/mol.K"
    , numberOfMoles = ConstantData "NUMBER OF MOLES" "n" 1 "moles"
    , temperature = ConstantData "TEMPERATURE(K)" "T" 273.2 "K"
    }

{-
Now, I can make a `RowI` for a constant input.
I use a `RowI` because this row cares about the `i`nput type.
I'll later use this row for each constant separately.

I get a pair of outputs:

- Top left cell of a constant's table. That is, the cell with that constant's name.
- The value of the constant.

Later, I'll use these outputs to relate the positions of tables on a sheet.

Notice that I use styles like `lightBlue` here. These styles are defined in the [Styles](#styles) section.
-}

constant :: (ToCellData a) => RowI (ConstantData a) (Ref (), Ref a)
constant = do
  refTopLeft <- columnF lightBlue constantName
  columnF_ lightBlue constantSymbol
  refValue <- columnF (lightBlue .& with2decimalDigits) constantValue
  columnF_ lightBlue constantUnits
  return (refTopLeft, refValue)

{-
#### volume and pressure values

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/valuesFormulas.png" width = "50%">

To fill this table, I'll take some data and combine it with the constants.
-}

newtype Volume = Volume {volume :: Double}

volumeData :: [Volume]
volumeData = Volume <$> [1 .. 10]

{-
To pass the constants references in a structured way, I make a helper type.
-}

type ConstantsRefs = Constants Ref

{-
Next, I define a function to produce a row for volume and pressure.
-}

values :: ConstantsRefs -> RowI Volume ()
values Constants{..} = do
  refVolume <- columnF alternatingColors volume
  let pressure' = gasConstant .* numberOfMoles .* temperature ./ refVolume
  columnF_ (alternatingColors .& with2decimalDigits) (const pressure')

{-
#### Constants' header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/constantsHeader.png" width = "50%">

I won't use records here. Instead, I'll put the names of the columns straight into the `Row`.

The outputs will be the coordinates of the top left cell and the top right cell of this table.
-}

constantsHeader :: Row (Ref (), Ref ())
constantsHeader = do
  let style :: FormatCell
      style = blue .& alignedCenter
  refTopLeft <- columnWF 20 style (const "constant")
  columnWF_ 8 style (const "symbol")
  columnF_ style (const "value")
  refTopRight <- columnWF 13 style (const "units")
  return (refTopLeft, refTopRight)

{-
#### Volume & Pressure header

<img src = "https://raw.githubusercontent.com/deemp/clerk/master/README/Example3/valuesHeader.png" width = "50%">

For this header, I'll also put the names of columns straight into a row.
-}

valuesHeader :: Row (Ref ())
valuesHeader = do
  refTopLeft <- columnWF 12 green (const "VOLUME (L)")
  columnWF_ 16 green (const "PRESSURE (atm)")
  return refTopLeft

{-
### Sheet builder

At last, I combine all rows.
-}

sheet :: Sheet ()
sheet = do
  start <- mkCoords 2 2
  (constantsHeaderTL, constantsHeaderTopRight) <- place start constantsHeader
  (gasTopLeft, gas) <- placeIn (constantsHeaderTL & row +~ 2) constants.gasConstant constant
  (nMolesTopLeft, nMoles) <- placeIn (gasTopLeft & row +~ 1) constants.numberOfMoles constant
  temperature <- snd <$> placeIn (nMolesTopLeft & row +~ 1) constants.temperature constant
  valuesHeaderTopLeft <- place (constantsHeaderTopRight & col +~ 2) valuesHeader
  placeIns (valuesHeaderTopLeft & row +~ 2) volumeData (values $ Constants gas nMoles temperature)

{-
### Styles

I used several styles to format the tables. This is how these styles are defined.
-}

blue, lightBlue, green, lightGreen :: FormatCell
blue = mkColor (hex @"#FF99CCFF")
lightBlue = mkColor (hex @"#90CCFFFF")
green = mkColor (hex @"#FF00FF00")
lightGreen = mkColor (hex @"#90CCFFCC")

alternatingColors :: FormatCell
alternatingColors index = (if even index then lightGreen else lightBlue) index

{-
Additionally, I compose an `FCTransform` for the number format.
Such a transform is used to accumulate cell formatting.
-}

with2decimalDigits :: FCTransform
with2decimalDigits fcTransform =
  fcTransform & X.formattedFormat %~ X.formatNumberFormat ?~ X.StdNumberFormat X.Nf2Decimal

{-
And I make a transform for centering the cell content.
-}

alignedCenter :: FCTransform
alignedCenter = horizontalAlignment X.CellHorizontalAlignmentCenter

{-
### Result

Finally, I write the result and get the spreadsheet like the one at the beginning of [Example 3](#example-3).
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