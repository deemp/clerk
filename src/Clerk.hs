{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | @Clerk@ library
module Clerk (
  -- * Coords
  -- $Coords
  Coords,
  coords,
  ToCoords (..),
  FromCoords (..),

  -- * Cell references
  -- $CellRef
  CellRef,
  getCol,
  getRow,
  overCol,
  overRow,
  unsafeChangeCellRefType,

  -- * Cell formatting
  -- $Formatting
  InputIndex,
  FormatCell,
  CellTemplate,
  FormattedMap,
  FMTransform,
  WSTransform,
  Transform,
  FCTransform,
  horizontalAlignment,
  mkColor,
  blank,
  ToARGB (..),

  -- * Templates
  -- $Templates
  RowBuilder (..),
  RowBuilder',
  Template (..),
  -- runBuilder,
  -- evalBuilder,
  -- execBuilder,
  -- RenderTemplate,
  -- RenderBuilderInputs,
  -- RenderBuilderInput,
  -- renderBuilderInputs,
  -- renderTemplate,

  -- * Columns
  -- $Columns
  ColumnsProperties (..),
  columnWidthCell,
  columnWidth,
  columnWidth_,
  column,
  column_,

  -- * Sheet builder
  -- $SheetBuilder
  SheetBuilder (..),
  placeInputs,
  placeInputs_,
  placeInput,
  placeInput_,

  -- * Expressions
  -- $Expressions
  Expr (..),
  ToExpr (..),
  ArithmeticOperator,
  (|+|),
  (|-|),
  (|*|),
  (|/|),
  (|:|),
  (|^|),
  (|$|),
  (+>),

  -- * Cells
  -- $Cells
  CellData (CellEmpty),
  ToCellData (..),

  -- * Produce xlsx
  -- $Xlsx
  composeXlsx,
) where

import Codec.Xlsx qualified as X
import Codec.Xlsx.Formatted qualified as X
import Control.Lens (Identity (runIdentity), (%~), (&), (?~))
import Control.Lens.Operators ((.~))
import Control.Monad.State (
  MonadState,
  StateT (StateT),
  evalStateT,
  get,
  gets,
  modify,
  void,
 )
import Control.Monad.Trans.Writer (execWriter, runWriter)
import Control.Monad.Writer (MonadWriter (..), Writer)
import Data.Char (toUpper)
import Data.Default (Default (..))
import Data.Foldable (Foldable (..))
import Data.List (intercalate)
import Data.Map.Strict qualified as Map (Map, insert)
import Data.Maybe (isJust, maybeToList)
import Data.Text qualified as T

-- TODO Allow sheet addresses

-- TODO Make formulas aware of the current sheet
-- Or, make just formula printers aware so that they don't print the full address
-- when referring to data on the same sheet

{- FOURMOLU_DISABLE -}
-- $Coords
{- FOURMOLU_ENABLE -}

-- | Coords of a cell
data Coords = Coords {row :: Int, col :: Int}

coords :: Int -> Int -> Coords
coords = Coords

class ToCoords a where
  toCoords :: a -> Coords

class FromCoords a where
  fromCoords :: Coords -> a

instance ToCoords Coords where
  toCoords :: Coords -> Coords
  toCoords = id

instance FromCoords Coords where
  fromCoords :: Coords -> Coords
  fromCoords = id

instance Show Coords where
  show :: Coords -> String
  show (Coords{..}) = toAlphaNumeric col <> show row

instance Num Coords where
  (+) :: Coords -> Coords -> Coords
  (+) (Coords r1 c1) (Coords r2 c2) = Coords (r1 + r2) (c1 + c2)
  (*) :: Coords -> Coords -> Coords
  (*) (Coords r1 c1) (Coords r2 c2) = Coords (r1 * r2) (c1 * c2)
  (-) :: Coords -> Coords -> Coords
  (-) (Coords r1 c1) (Coords r2 c2) = Coords (r1 - r2) (c1 - c2)
  abs :: Coords -> Coords
  abs (Coords r1 c1) = Coords (abs r1) (abs c1)
  signum :: Coords -> Coords
  signum (Coords r1 c1) = Coords (signum r1) (signum c1)
  fromInteger :: Integer -> Coords
  fromInteger x = Coords (fromIntegral (abs x)) (fromIntegral (abs x))

-- | Letters that can be used in column indices
alphabet :: [Char]
alphabet = ['A' .. 'Z']

-- | Translate a number into an alphanumeric representation. Relevant for columns
toAlphaNumeric :: Int -> String
toAlphaNumeric x = f "" (x - 1)
 where
  new :: Int -> String -> String
  new cur acc = [alphabet !! (cur `mod` 26)] <> acc
  f :: String -> Int -> String
  f acc cur = if cur `div` 26 > 0 then f (new cur acc) (cur `div` 26 - 1) else new cur acc

{-
>>>toLetters <$> [1, 26, 27, 52, 78]
["A","Z","AA","AZ","BZ"]
-}

-- {- FOURMOLU_DISABLE -}

-- $CellRef
-- {\- FOURMOLU_ENABLE -\}

-- | A typed reference to a cell.
--
-- The user is responsible for setting the necessary cell type.
--
-- The type prevents operations between cell references with incompatible types.
--
-- >>>str = CellRef (Coords 1 1) :: CellRef String
-- >>> str |+| str
-- No instance for (Num String) arising from a use of ‘|+|’
--
-- When necessary, the user may change the cell reference type via 'unsafeChangeCellRefType'
--
-- >>>int = CellRef (Coords 1 1) :: CellRef Int
-- >>>double = CellRef (Coords 2 5) :: CellRef Double
-- >>>unsafeChangeCellRefType int |+| double
-- A1+E2
newtype CellRef a = CellRef {unCellRef :: Coords}
  deriving newtype (Num)

instance ToCoords (CellRef a) where
  toCoords :: CellRef a -> Coords
  toCoords = unCellRef

instance FromCoords (CellRef a) where
  fromCoords :: Coords -> CellRef a
  fromCoords = CellRef

-- | Get a column number
getCol :: ToCoords a => a -> Int
getCol = col . toCoords

-- | Get a row number
getRow :: ToCoords a => a -> Int
getRow = row . toCoords

-- | Apply a function over a column of a coordinate
overCol :: ToCoords a => (Int -> Int) -> a -> Coords
overCol f (toCoords -> Coords row col) = Coords row (f col)

-- | Apply a function over a row of a coordinate
overRow :: ToCoords a => (Int -> Int) -> a -> Coords
overRow f (toCoords -> Coords row col) = Coords (f row) col

-- | Change the type of a cell reference. Use with caution!
--
-- The type variables in the @forall@ clause are swapped for the conveniece of type applications
unsafeChangeCellRefType :: forall b a. CellRef a -> CellRef b
unsafeChangeCellRefType (CellRef c) = CellRef c

{- FOURMOLU_DISABLE -}
-- $Formatting
{- FOURMOLU_ENABLE -}

-- | Index of an input
type InputIndex = Int

-- | Format a single cell depending on its coordinates, index, and data
type FormatCell = forall a. ToCoords a => a -> InputIndex -> CellData -> X.FormattedCell

-- | Template of a cell with contents, style, column properties
data CellTemplate input output = CellTemplate
  { mkOutput :: input -> output
  , fmtCell :: FormatCell
  , columnsProperties :: Maybe X.ColumnsProperties
  }

-- | Map of coordinates to cell formatting
type FormattedMap = Map.Map (X.RowIndex, X.ColumnIndex) X.FormattedCell

-- | Transform of a map that maps coordinates to cell formatting
type FMTransform = FormattedMap -> FormattedMap

-- | Transform of a worksheet
type WSTransform = X.Worksheet -> X.Worksheet

-- | Combined: a transform of a map of formats and a transform of a worksheet
data Transform = Transform {fmTransform :: FMTransform, wsTransform :: WSTransform}

instance Semigroup Transform where
  (<>) :: Transform -> Transform -> Transform
  (Transform a1 b1) <> (Transform a2 b2) = Transform (a2 . a1) (b2 . b1)

instance Monoid Transform where
  mempty :: Transform
  mempty = Transform id id

instance Default Transform where
  def :: Transform
  def = mempty

-- | something that can be turned into ARGB
class ToARGB a where
  toARGB :: a -> String

-- | Make a 'FormatCell' for a single color
--
-- @show@ on the input should translate into an @ARGB@ color. See 'XS.Color'
mkColor :: ToARGB a => a -> FormatCell
mkColor color _ _ cd =
  X.def
    & X.formattedCell .~ dataCell cd
    & X.formattedFormat
      .~ ( X.def
            & X.formatFill
              ?~ ( X.def
                    & X.fillPattern
                      ?~ ( X.def
                            & ( X.fillPatternFgColor
                                  ?~ (X.def & X.colorARGB ?~ T.pack (toARGB color))
                              )
                            & ( X.fillPatternType
                                  ?~ X.PatternTypeSolid
                              )
                         )
                 )
         )

-- | A 'FormatCell' that produces a cell with the given data
blank :: FormatCell
blank _ _ cd = X.def & X.formattedCell .~ dataCell cd

-- | Transform of a formatted cell
type FCTransform = X.FormattedCell -> X.FormattedCell

-- | Apply 'FCTransform' to a 'FormatCell' to get a new 'FormatCell'
(+>) :: FormatCell -> FCTransform -> FormatCell
fc +> ft = \coords_ idx cd -> ft $ fc coords_ idx cd

infixl 5 +>

-- | Get a 'FCTransform' with a given horizontal alignment in a cell
horizontalAlignment :: X.CellHorizontalAlignment -> FCTransform
horizontalAlignment alignment fc =
  fc
    & X.formattedFormat
      %~ ( \ff ->
            ff
              & X.formatAlignment
                ?~ ( X.def & X.alignmentHorizontal ?~ alignment
                   )
         )

{- FOURMOLU_DISABLE -}
-- $Templates
{- FOURMOLU_ENABLE -}

-- | Template for multiple cells
newtype Template input output = Template [CellTemplate input output]
  deriving (Semigroup, Monoid)

-- | Allows to describe how to build a template for a row
newtype RowBuilder input output a = RowBuilder {unBuilder :: StateT Coords (Writer (Template input output)) a}
  deriving (Functor, Applicative, Monad, MonadState Coords, MonadWriter (Template input output))

type RowBuilder' input a = RowBuilder input CellData a

-- | Run builder on given coordinates. Get a result and a template
runBuilder :: RowBuilder input output a -> Coords -> (a, Template input output)
runBuilder builder coord = runWriter (evalStateT (unBuilder builder) coord)

-- | Run builder on given coordinates. Get a template
evalBuilder :: RowBuilder input output a -> Coords -> Template input output
evalBuilder builder coord = snd $ runBuilder builder coord

-- | Run builder on given coordinates. Get a result
execBuilder :: RowBuilder input output a -> Coords -> a
execBuilder builder coord = fst $ runBuilder builder coord

type RenderTemplate m input output = (Monad m, ToCellData output) => Coords -> InputIndex -> input -> Template input output -> m Transform
type RenderBuilderInputs m input output a = (Monad m, ToCellData output) => RowBuilder input output a -> [input] -> m (Transform, a)
type RenderBuilderInput m input output a = (Monad m, ToCellData output) => RowBuilder input output a -> input -> m (Transform, a)

-- | Render a builder with given coords and inputs. Return the result calculated using the topmost row
renderBuilderInputs :: (Monad m, ToCellData output) => Coords -> RenderTemplate m input output -> RenderBuilderInputs m input output a
renderBuilderInputs offset render builder inputs = ret
 where
  ts =
    [ (coord, template)
    | row <- [0 .. length inputs]
    , let coord = offset + Coords{row, col = 0}
          template = evalBuilder builder coord
    ]
  -- result obtained from the top row
  a = execBuilder builder (offset + Coords{row = 0, col = 0})
  transform =
    fold
      <$> sequenceA
        ( zipWith3
            ( \input inputIdx (coord, template) ->
                render coord inputIdx input template
            )
            inputs
            [0 ..]
            ts
        )
  ret = (,a) <$> transform

-- | Render a template with a given offset, input index and input
renderTemplate :: RenderTemplate m input output
renderTemplate Coords{..} inputIdx input (Template columns) = return $ fold ps
 where
  ps =
    zipWith
      ( \columnIdx mk ->
          let
            CellTemplate{..} = mk
            cd' = toCellData (mkOutput input)
            col' = (col + columnIdx)
            coords' = Coords row col'
            c = fmtCell coords' inputIdx cd'
            fmTransform = Map.insert (fromIntegral row, fromIntegral col') c
            wsTransform
              -- add column width only once
              | inputIdx == 0 = X.wsColumnsProperties %~ (\x -> x ++ maybeToList columnsProperties)
              | otherwise = id
           in
            def{fmTransform, wsTransform}
      )
      [0 ..]
      columns

{- FOURMOLU_DISABLE -}
-- $Columns
{- FOURMOLU_ENABLE -}

-- | Properties of a column
newtype ColumnsProperties = ColumnsProperties {unColumnsProperties :: X.ColumnsProperties}

instance Default ColumnsProperties where
  def :: ColumnsProperties
  def =
    ColumnsProperties
      X.ColumnsProperties
        { cpMin = 1
        , cpMax = 1
        , cpWidth = Nothing
        , cpStyle = Nothing
        , cpHidden = False
        , cpCollapsed = False
        , cpBestFit = False
        }

-- | A column with a possibly given width and cell format. Returns a cell reference
columnWidthCell :: forall a input output. Maybe Double -> FormatCell -> (input -> output) -> RowBuilder input output (CellRef a)
columnWidthCell width fmtCell mkOutput = do
  coords_ <- get
  let columnsProperties =
        Just $
          (unColumnsProperties def)
            { X.cpMin = coords_ & col
            , X.cpMax = coords_ & col
            , X.cpWidth = width
            }
  tell (Template [CellTemplate{fmtCell, mkOutput, columnsProperties}])
  cell <- gets CellRef
  modify (\x -> x{col = (x & col) + 1})
  return cell

-- | A column with a given width and cell format. Returns a cell reference
columnWidth :: ToCellData output => Double -> FormatCell -> (input -> output) -> RowBuilder input CellData (CellRef a)
columnWidth width fmtCell mkOutput = columnWidthCell (Just width) fmtCell (toCellData . mkOutput)

-- | A column with a given width and cell format
columnWidth_ :: ToCellData output => Double -> FormatCell -> (input -> output) -> RowBuilder input CellData ()
columnWidth_ width fmtCell mkOutput = void (columnWidth width fmtCell mkOutput)

-- | A column with a given cell format. Returns a cell reference
column :: ToCellData output => FormatCell -> (input -> output) -> RowBuilder input CellData (CellRef a)
column fmtCell mkOutput = columnWidthCell Nothing fmtCell (toCellData . mkOutput)

-- | A column with a given cell format
column_ :: ToCellData output => FormatCell -> (input -> output) -> RowBuilder input CellData ()
column_ fmtCell mkOutput = void (column fmtCell mkOutput)

-- | Produce a transform and a result from a template renderer, inputs, and a builder
composeTransformAndResult :: forall a input output. ToCellData output => RenderTemplate Identity input output -> Coords -> [input] -> RowBuilder input output a -> (Transform, a)
composeTransformAndResult renderTemplate' offset input builder = runIdentity $ renderBuilderInputs offset renderTemplate' builder input

-- | Produce a result from a default template renderer, inputs, and a builder
defaultComposeTransformAndResult :: ToCellData output => Coords -> [input] -> RowBuilder input output a -> (Transform, a)
defaultComposeTransformAndResult = composeTransformAndResult renderTemplate

{- FOURMOLU_DISABLE -}
-- $SheetBuilder
{- FOURMOLU_ENABLE -}

-- | A builder to compose the results of 'RowBuilder's
newtype SheetBuilder a = SheetBuilder {unSheetBuilder :: Writer Transform a}
  deriving (Functor, Applicative, Monad, MonadWriter Transform)

-- | Starting at given coordinates, place rows of data made from a list of inputs according to a row builder. Return the result of the row builder.
placeInputs :: (ToCellData output, ToCoords c) => c -> [input] -> RowBuilder input output a -> SheetBuilder a
placeInputs (toCoords -> coords_) inputs b = do
  let transformResult = defaultComposeTransformAndResult coords_ inputs b
  tell (fst transformResult)
  return (snd transformResult)

-- | Starting at given coordinates, place a row of data made from a single input according to a row builder. Return the result of the row builder.
placeInput :: (ToCellData output, ToCoords c) => c -> input -> RowBuilder input output a -> SheetBuilder a
placeInput coords_ input = placeInputs coords_ [input]

-- | Starting at given coordinates, place rows of data made from a list of inputs according to a row builder.
placeInputs_ :: (ToCellData output, ToCoords c) => c -> [input] -> RowBuilder input output a -> SheetBuilder ()
placeInputs_ coords_ inputs b = void (placeInputs coords_ inputs b)

-- | Starting at given coordinates, place a row of data made from a single input according to a row builder.
placeInput_ :: (ToCellData output, ToCoords c) => c -> input -> RowBuilder input output a -> SheetBuilder ()
placeInput_ coords_ input = placeInputs_ coords_ [input]

{- FOURMOLU_DISABLE -}
-- $Expressions
{- FOURMOLU_ENABLE -}

-- | Expression syntax
data Expr t
  = Add (Expr t) (Expr t)
  | Sub (Expr t) (Expr t)
  | Mul (Expr t) (Expr t)
  | Div (Expr t) (Expr t)
  | Power (Expr t) (Expr t)
  | Function String [Expr t]
  | Range (Expr t) (Expr t)
  | ExprCell (CellRef t)

-- | Something that can be turned into an expression
class ToExpr v where
  toExpr :: v -> Expr t

instance ToExpr (CellRef a) where
  toExpr :: CellRef a -> Expr t
  toExpr (CellRef c) = ExprCell (CellRef c)

instance ToExpr Coords where
  toExpr :: Coords -> Expr t
  toExpr c = ExprCell (CellRef c)

instance ToExpr (Expr a) where
  toExpr :: Expr a -> Expr b
  toExpr (Add l r) = Add (toExpr l) (toExpr r)
  toExpr (Sub l r) = Sub (toExpr l) (toExpr r)
  toExpr (Mul l r) = Mul (toExpr l) (toExpr r)
  toExpr (Div l r) = Div (toExpr l) (toExpr r)
  toExpr (Power b p) = Power (toExpr b) (toExpr p)
  toExpr (Function name args) = Function name (toExpr <$> args)
  toExpr (Range l r) = Range (toExpr l) (toExpr r)
  toExpr (ExprCell (CellRef c)) = ExprCell (CellRef c)

showOp2 :: (Show a, Show b) => String -> a -> b -> String
showOp2 operator c1 c2 = show c1 <> operator <> show c2

mkOp2 :: (ToExpr a, ToExpr b) => (Expr t -> Expr t -> Expr t) -> a -> b -> Expr t
mkOp2 f c1 c2 = f (toExpr c1) (toExpr c2)

mkNumOp2 :: (Num t, ToExpr a, ToExpr b) => (Expr t -> Expr t -> Expr t) -> a -> b -> Expr t
mkNumOp2 = mkOp2

-- | Assemble a range expression
(|:|) :: CellRef a -> CellRef b -> Expr c
(|:|) = mkOp2 Range

infixr 5 |:|

-- | A type for arithmetic operators
type ArithmeticOperator a b c = (Num a, ToExpr (b a), ToExpr (c a)) => b a -> c a -> Expr a

-- | Assemble an addition expression
(|+|) :: ArithmeticOperator a b c
(|+|) = mkNumOp2 Add

infixl 6 |+|

-- | Assemble a subtraction expression
(|-|) :: ArithmeticOperator a b c
(|-|) = mkNumOp2 Sub

infixl 6 |-|

-- | Assemble a division expression
(|/|) :: ArithmeticOperator a b c
(|/|) = mkNumOp2 Div

infixl 7 |/|

-- | Assemble a multiplication expression
(|*|) :: ArithmeticOperator a b c
(|*|) = mkNumOp2 Mul

infixl 6 |*|

-- | Assemble a multiplication expression
(|^|) :: ArithmeticOperator a b c
(|^|) = mkNumOp2 Power

infixr 8 |^|

-- | Assemble a function expression
(|$|) :: ToExpr a => String -> [a] -> Expr t
(|$|) n as = Function (toUpper <$> n) (toExpr <$> as)

infixr 0 |$|

instance Show (Expr t) where
  show :: Expr t -> String
  show (Add c1 c2) = showOp2 "+" c1 c2
  show (Sub c1 c2) = showOp2 "-" c1 c2
  show (Mul c1 c2) = showOp2 "*" c1 c2
  show (Div c1 c2) = showOp2 "/" c1 c2
  show (Power c1 c2) = showOp2 "^" c1 c2
  show (Range c1 c2) = showOp2 ":" c1 c2
  show (ExprCell (CellRef e)) = show e
  show (Function n as) = n <> "(" <> intercalate "," (show <$> as) <> ")"

{- FOURMOLU_DISABLE -}
-- $Cells
{- FOURMOLU_ENABLE -}

-- | A union of what can be inside a cell
data CellData
  = CellFormula X.CellFormula
  | CellValue X.CellValue
  | CellComment X.Comment
  | CellEmpty

instance Default CellData where
  def :: CellData
  def = CellEmpty

-- | Convert some CellRef component into a cell
dataCell :: CellData -> X.Cell
dataCell cd =
  X.def
    & case cd of
      CellValue d -> X.cellValue ?~ d
      CellFormula d -> X.cellFormula ?~ d
      CellComment d -> X.cellComment ?~ d
      CellEmpty -> X.def

-- | Something that can be turned into 'CellData'
class ToCellData a where
  toCellData :: a -> CellData

instance ToCellData String where
  toCellData :: String -> CellData
  toCellData = CellValue . X.CellText . T.pack

instance ToCellData Int where
  toCellData :: Int -> CellData
  toCellData = CellValue . X.CellDouble . fromIntegral

instance ToCellData Double where
  toCellData :: Double -> CellData
  toCellData = CellValue . X.CellDouble

instance ToCellData Bool where
  toCellData :: Bool -> CellData
  toCellData = CellValue . X.CellBool

instance ToCellData CellData where
  toCellData :: CellData -> CellData
  toCellData = id

instance ToCellData (Expr a) where
  toCellData :: Expr a -> CellData
  toCellData e =
    CellFormula
      X.CellFormula
        { X._cellfAssignsToName = False
        , X._cellfCalculate = True
        , X._cellfExpression = X.NormalFormula $ X.Formula $ T.pack $ show e
        }

{- FOURMOLU_DISABLE -}
-- $Xlsx
{- FOURMOLU_ENABLE -}

-- | Compose an @xlsx@ from a list of sheet names and builders
composeXlsx :: [(T.Text, SheetBuilder ())] -> X.Xlsx
composeXlsx sheetBuilders = workBook'
 where
  getTransform x = execWriter $ unSheetBuilder x
  workBook = X.formatWorkbook ((\(name, tf') -> (name, (getTransform tf' & fmTransform) X.def)) <$> sheetBuilders) X.def
  filterWidths ws = ws & X.wsColumnsProperties %~ filter (isJust . X.cpWidth)
  workBook' =
    workBook
      & X.xlSheets
        %~ \sheets -> zipWith (\x (name, ws) -> (name, (getTransform x & wsTransform) ws & filterWidths)) (snd <$> sheetBuilders) sheets
