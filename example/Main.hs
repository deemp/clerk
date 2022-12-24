module Main (main) where

import Clerk (
    Builder,
    Cell (unCell),
    CellData,
    Coords (Coords),
    FCTransform,
    FormatCell,
    SheetBuilder (unSheetBuilder),
    ToCellData,
    column,
    columnWidth,
    columnWidth_,
    column_,
    composeXlsx,
    ex,
    horizontalAlignment,
    mkColorStyle,
    overCol,
    overRow,
    placeInput,
    placeInputs_,
    (<|),
    (|*|),
    (|/|),
 )
import Codec.Xlsx qualified as X
import Codec.Xlsx.Formatted qualified as X
import Control.Lens ((%~), (&), (?~))
import Control.Monad.Writer (execWriter)
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)

{- Example. Data -}

data ConstantsData a = ConstantsData
    { name :: String
    , symbol :: String
    , value :: a
    , units :: String
    }

data ConstantsValues = ConstantsValues
    { gas :: Cell Double
    , nMoles :: Cell Double
    , temperature :: Cell Double
    }

data ConstantsInput = ConstantsInput
    { gas :: ConstantsData Double
    , nMoles :: ConstantsData Double
    , temperature :: ConstantsData Double
    }

constants :: ConstantsInput
constants =
    ConstantsInput
        { gas = ConstantsData "GAS CONSTANT" "R" 0.08206 "L.atm/mol.K"
        , nMoles = ConstantsData "NUMBER OF MOLES" "n" 1 "moles"
        , temperature = ConstantsData "TEMPERATURE(K)" "T" 273.2 "K"
        }

data ConstantsHeader = ConstantsHeader
    { hConstant :: String
    , hSymbol :: String
    , hValue :: String
    , hUnits :: String
    }

constantsHeader :: ConstantsHeader
constantsHeader =
    ConstantsHeader
        { hConstant = "constant"
        , hSymbol = "symbol"
        , hValue = "value"
        , hUnits = "units"
        }

data ValuesHeader = ValuesHeader
    { hVolume :: String
    , hPressure :: String
    }

valuesHeader :: ValuesHeader
valuesHeader =
    ValuesHeader
        { hVolume = "VOLUME (L)"
        , hPressure = "PRESSURE (atm)"
        }

newtype Volume = Volume
    { volume :: Double
    }

volumeData :: [Volume]
volumeData = take 10 $ Volume <$> [1 ..]

{- Example. Styles -}

data Colors = Colors
    { lightBlue :: T.Text
    , lightGreen :: T.Text
    , blue :: T.Text
    , green :: T.Text
    }

colors :: Colors
colors =
    Colors
        { lightGreen = "90CCFFCC"
        , lightBlue = "90CCFFFF"
        , blue = "FF99CCFF"
        , green = "FF00FF00"
        }

colorBlue :: FormatCell
colorBlue = mkColorStyle colors.blue

colorLightBlue :: FormatCell
colorLightBlue = mkColorStyle colors.lightBlue

colorGreen :: FormatCell
colorGreen = mkColorStyle colors.green

colorMixed :: FormatCell
colorMixed coords idx = mkColorStyle (if even idx then colors.lightGreen else colors.lightBlue) coords idx

nf2decimal :: FCTransform
nf2decimal fc = fc & X.formattedFormat %~ (\ff -> ff & X.formatNumberFormat ?~ X.StdNumberFormat X.Nf2Decimal)

alignCenter :: FCTransform
alignCenter = horizontalAlignment X.CellHorizontalAlignmentCenter

{- Example. Builders -}

constantsHeaderBuilder :: Builder ConstantsHeader CellData (Coords, Coords)
constantsHeaderBuilder = do
    tl <- columnWidth 20 (alignCenter <| colorBlue) hConstant
    columnWidth_ 8 (alignCenter <| colorBlue) hSymbol
    column_ (alignCenter <| colorBlue) hValue
    tr <- column (alignCenter <| colorBlue) hUnits
    return (unCell tl, unCell tr)

constantBuilder :: forall a. ToCellData a => Builder (ConstantsData a) CellData (Coords, Cell a)
constantBuilder = do
    topLeft <- column colorLightBlue name
    column_ colorLightBlue symbol
    value <- column (nf2decimal <| colorLightBlue) value
    column_ colorLightBlue units
    return (unCell topLeft, value)

valuesHeaderBuilder :: Builder ValuesHeader CellData Coords
valuesHeaderBuilder = do
    tl <- column colorGreen hVolume
    columnWidth_ 16 colorGreen hPressure
    return (unCell tl)

valuesBuilder :: ConstantsValues -> Builder Volume CellData ()
valuesBuilder cv = do
    volume' <- column colorMixed volume
    let pressure' = ex cv.gas |*| ex cv.nMoles |*| ex cv.temperature |/| ex volume'
    column_ (nf2decimal <| colorMixed) (const pressure')

full :: SheetBuilder ()
full = do
    (constantsHeaderTL, constantsHeaderTR) <- placeInput (Coords 2 2) constantsHeader constantsHeaderBuilder
    (gasTL, gas) <- placeInput (overRow (+ 2) constantsHeaderTL) constants.gas constantBuilder
    (nMolesTL, nMoles) <- placeInput (overRow (+ 1) gasTL) constants.nMoles constantBuilder
    temperature <- snd <$> placeInput (overRow (+ 1) nMolesTL) constants.temperature constantBuilder
    valuesHeaderTL <- placeInput (overCol (+ 2) constantsHeaderTR) valuesHeader valuesHeaderBuilder
    placeInputs_ (overRow (+ 2) valuesHeaderTL) volumeData (valuesBuilder $ ConstantsValues{..})

writeWorksheet :: SheetBuilder a -> String -> IO ()
writeWorksheet tb name = do
    ct <- getPOSIXTime
    let
        tb' = execWriter $ unSheetBuilder tb
        xlsx = composeXlsx [("List 1", tb')]
    L.writeFile ("example-" <> name <> ".xlsx") $ X.fromXlsx ct xlsx

writeEx :: IO ()
writeEx = writeWorksheet full "1"

main :: IO ()
main = writeEx