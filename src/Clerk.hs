{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Clerk (
  Builder,
  Cell (unCell),
  CellData,
  Coords (Coords),
  FCTransform,
  FormatCell,
  ToCellData,
  SheetBuilder (..),
  column,
  columnWidth,
  columnWidth_,
  column_,
  composeXlsx,
  ex,
  ex',
  horizontalAlignment,
  mkColorStyle,
  overCol,
  overRow,
  placeInput,
  placeInputs_,
  (|+|),
  (|-|),
  (|*|),
  (|/|),
  (|:|),
  (|^|),
  (|$|),
  (<|),
  Expr (..),
) where

import qualified Codec.Xlsx as X
import qualified Codec.Xlsx.Formatted as X
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
import qualified Data.Map.Strict as Map (Map, insert)
import Data.Maybe (isJust, maybeToList)
import qualified Data.Text as T

-- Coords

-- TODO Allow sheet addresses

-- | Coords of a cell
data Coords = Coords {row :: Int, col :: Int}

instance Show Coords where
  show :: Coords -> String
  show (Coords{..}) = toLetters col <> show row

alphabet :: [String]
alphabet = (: "") <$> ['A' .. 'Z']

toLetters :: Int -> String
toLetters x = f "" (x - 1)
 where
  new :: Int -> String -> String
  new cur acc = alphabet !! (cur `mod` 26) <> acc
  f :: String -> Int -> String
  f acc cur = if cur `div` 26 > 0 then f (new cur acc) (cur `div` 26 - 1) else new cur acc

{-
>>>toLetters <$> [1, 26, 27, 52, 78]
["A","Z","AA","AZ","BZ"]
-}

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

-- Cell

-- | Index of an input
type Index = Int

-- | Format a single cell depending on its coordinates, index, and data
type FormatCell = Coords -> Index -> CellData -> X.FormattedCell

-- | Cell with contents, style, column props
data CellTemplate input output = CellTemplate
  { mkOutput :: input -> output
  , format :: FormatCell
  , columnsProperties :: Maybe X.ColumnsProperties
  }

-- Transforms

type FormattedMap = Map.Map (X.RowIndex, X.ColumnIndex) X.FormattedCell
type FMTransform = FormattedMap -> FormattedMap
type WSTransform = X.Worksheet -> X.Worksheet

-- | A transform of the map of formats and a transform of a worksheet
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

-- Template

-- | Template for multiple cells
newtype Template input output = Template [CellTemplate input output]
  deriving (Semigroup, Monoid)

-- Builder

-- | A builder
newtype Builder input output a = Builder {unBuilder :: StateT Coords (Writer (Template input output)) a}
  deriving (Functor, Applicative, Monad, MonadState Coords, MonadWriter (Template input output))

-- | Run builder on given coordinates. Get a result and a template
runBuilder :: Builder input output a -> Coords -> (a, Template input output)
runBuilder builder coord = runWriter (evalStateT (unBuilder builder) coord)

-- | Run builder on given coordinates. Get a template
evalBuilder :: Builder input output a -> Coords -> Template input output
evalBuilder builder coord = snd $ runBuilder builder coord

-- | Run builder on given coordinates. Get a result
execBuilder :: Builder input output a -> Coords -> a
execBuilder builder coord = fst $ runBuilder builder coord

type RenderTemplate m input output = (Monad m, ToCellData output) => Coords -> Index -> input -> Template input output -> m Transform
type RenderBuilderInputs m input output a = (Monad m, ToCellData output) => Builder input output a -> [input] -> m (Transform, a)
type RenderBuilderInput m input output a = (Monad m, ToCellData output) => Builder input output a -> input -> m (Transform, a)

-- Render
-- meaning produce a transform

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
            c = format coords' inputIdx cd'
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

-- Columns

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

-- TODO fix doesn't work for non-first row
-- need to filter the final list

-- | Produce a column with a given style and width and get a cell
columnWidthCell :: forall a input output. Maybe Double -> FormatCell -> (input -> output) -> Builder input output (Cell a)
columnWidthCell width format mkOutput = do
  coords <- get
  let columnsProperties =
        Just $
          (unColumnsProperties def)
            { X.cpMin = coords & col
            , X.cpMax = coords & col
            , X.cpWidth = width
            }
  tell (Template [CellTemplate{format, mkOutput, columnsProperties}])
  cell <- gets Cell
  modify (\x -> x{col = (x & col) + 1})
  return cell

columnWidth :: ToCellData output => Double -> FormatCell -> (input -> output) -> Builder input CellData (Cell a)
columnWidth width fmtCell mkOutput = columnWidthCell (Just width) fmtCell (toCellData . mkOutput)

columnWidth_ :: ToCellData output => Double -> FormatCell -> (input -> output) -> Builder input CellData ()
columnWidth_ width fmtCell mkOutput = void (columnWidth width fmtCell mkOutput)

-- | Produce a column with a given style and get a cell
column :: ToCellData output => FormatCell -> (input -> output) -> Builder input CellData (Cell a)
column fmtCell mkOutput = columnWidthCell Nothing fmtCell (toCellData . mkOutput)

-- | Produce a column with a given style
column_ :: ToCellData output => FormatCell -> (input -> output) -> Builder input CellData ()
column_ fmtCell mkOutput = void (column fmtCell mkOutput)

-- | Produce a transform and a result from inputs and a builder
composeTransformResult :: forall a input output. ToCellData output => RenderTemplate Identity input output -> Coords -> [input] -> Builder input output a -> (Transform, a)
composeTransformResult renderTemplate' offset input builder = runIdentity $ renderBuilderInputs offset renderTemplate' builder input

-- | Produce a result
defaultTransformResult :: ToCellData output => Coords -> [input] -> Builder input output a -> (Transform, a)
defaultTransformResult = composeTransformResult renderTemplate

-- TODO
-- Store current sheet info for formulas

-- | Top monad to compose the results of Builders
newtype SheetBuilder a = SheetBuilder {unSheetBuilder :: Writer Transform a}
  deriving (Functor, Applicative, Monad, MonadWriter Transform)

class Functor a => Discardable a where
  discard :: a b -> a ()

placeInputs :: ToCellData output => Coords -> [input] -> Builder input output a -> SheetBuilder a
placeInputs offset inputs b = do
  let transformResult = defaultTransformResult offset inputs b
  tell (fst transformResult)
  return (snd transformResult)

placeInput :: ToCellData output => Coords -> input -> Builder input output a -> SheetBuilder a
placeInput coords input = placeInputs coords [input]

placeInputs_ :: ToCellData output => Coords -> [input] -> Builder input output a -> SheetBuilder ()
placeInputs_ coords inputs b = void (placeInputs coords inputs b)

placeInput_ :: ToCellData output => Coords -> input -> Builder input output a -> SheetBuilder ()
placeInput_ coords input = placeInputs_ coords [input]

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

{- Lib. Formulas -}

-- | Formula expressions
data Expr t
  = Add (Expr t) (Expr t)
  | Sub (Expr t) (Expr t)
  | Mul (Expr t) (Expr t)
  | Div (Expr t) (Expr t)
  | Function String [Expr t]
  | Range (Expr t) (Expr t)
  | ExprCell (Cell t)
  deriving (Functor)

-- | Change phantom type of an Expr
ex' :: forall b a. Expr a -> Expr b
ex' = toExpr

-- | Something that can be turned into an expression
class ToExpr v where
  toExpr :: v -> Expr t

instance ToExpr (Cell a) where
  toExpr :: Cell a -> Expr t
  toExpr (Cell c) = ExprCell (Cell c)

instance ToExpr Coords where
  toExpr :: Coords -> Expr t
  toExpr c = ExprCell (Cell c)

toExprCell :: Cell a -> Coords
toExprCell (Cell c1) = c1

instance ToExpr (Expr a) where
  toExpr :: Expr a -> Expr b
  toExpr (Add l r) = Add (toExpr l) (toExpr r)
  toExpr (Sub l r) = Sub (toExpr l) (toExpr r)
  toExpr (Mul l r) = Mul (toExpr l) (toExpr r)
  toExpr (Div l r) = Div (toExpr l) (toExpr r)
  toExpr (Function name args) = Function name (toExpr <$> args)
  toExpr (Range l r) = Range (toExpr l) (toExpr r)
  toExpr (ExprCell (Cell c)) = ExprCell (Cell c)

showOp2 :: (Show a, Show b) => String -> a -> b -> String
showOp2 operator c1 c2 = show c1 <> operator <> show c2

mkOp2 :: (ToExpr a, ToExpr b) => (Expr t -> Expr t -> Expr t) -> a -> b -> Expr t
mkOp2 f c1 c2 = f (toExpr c1) (toExpr c2)

mkNumOp2 :: (Num t, ToExpr a, ToExpr b) => (Expr t -> Expr t -> Expr t) -> a -> b -> Expr t
mkNumOp2 = mkOp2

-- | Assemble a range expression
(|:|) :: Cell a -> Cell b -> Expr c
(|:|) = mkOp2 Range

infixr 5 |:|

-- | Assemble an addition expression
(|+|) :: Num a => Expr a -> Expr a -> Expr a
(|+|) = mkNumOp2 Add

infixl 6 |+|

-- | Assemble a subtraction expression
(|-|) :: Num a => Expr a -> Expr a -> Expr a
(|-|) = mkNumOp2 Sub

infixl 6 |-|

-- | Assemble a division expression
(|/|) :: Num a => Expr a -> Expr a -> Expr a
(|/|) = mkNumOp2 Div

infixl 7 |/|

-- | Assemble a multiplication expression
(|*|) :: Num a => Expr a -> Expr a -> Expr a
(|*|) = mkNumOp2 Mul

infixl 6 |*|

-- | Assemble a multiplication expression
(|^|) :: Num a => Expr a -> Expr a -> Expr a
(|^|) = mkNumOp2 Mul

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
  show (Range c1 c2) = showOp2 ":" c1 c2
  show (ExprCell (Cell e)) = show e
  show (Function n as) = n <> "(" <> intercalate "," (show <$> as) <> ")"

-- | Coordinates of a cell with a given phantom type
newtype Cell a = Cell {unCell :: Coords} deriving (Functor)

cellCol :: Cell a -> Int
cellCol (Cell c) = c & col

cellRow :: Cell a -> Int
cellRow (Cell c) = c & row

overCol :: (Int -> Int) -> Coords -> Coords
overCol f (Coords row col) = Coords row (f col)

overRow :: (Int -> Int) -> Coords -> Coords
overRow f (Coords row col) = Coords (f row) col

instance Num (Cell a) where
  (+) :: Cell a -> Cell a -> Cell a
  (+) (Cell c1) (Cell c2) = Cell (c1 + c2)
  (*) :: Cell a -> Cell a -> Cell a
  (*) (Cell c1) (Cell c2) = Cell (c1 * c2)
  (-) :: Cell a -> Cell a -> Cell a
  (-) (Cell c1) (Cell c2) = Cell (c1 - c2)
  abs :: Cell a -> Cell a
  abs (Cell c1) = Cell (abs c1)
  signum :: Cell a -> Cell a
  signum (Cell c1) = Cell (signum c1)
  fromInteger :: Integer -> Cell a
  fromInteger x = Cell (fromInteger x)

-- | Convert a typed cell to an expression
ex :: Cell a -> Expr a
ex = toExpr

{- Lib.Example.Typechecks
>>>str = ex (Cell (Coords 1 1)) :: Expr String
>>> str |+| str
No instance for (Num String) arising from a use of `|+|'
In the expression: str |+| str
NOW In an equation for `it_a1TViD': it_a1TViD = str |+| str

>>>int = ex (Cell (Coords 1 1)) :: Expr Int
>>>double = ex (Cell (Coords 2 5)) :: Expr Double
>>>ex' int |+| double
A1+E2
-}

mkColorStyle :: T.Text -> FormatCell
mkColorStyle color _ _ cd =
  X.def
    & X.formattedCell .~ dataCell cd
    & X.formattedFormat
      .~ ( X.def
            & X.formatFill
              ?~ ( X.def
                    & X.fillPattern
                      ?~ ( X.def
                            & ( X.fillPatternFgColor
                                  ?~ (X.def & X.colorARGB ?~ color)
                              )
                            & ( X.fillPatternType
                                  ?~ X.PatternTypeSolid
                              )
                         )
                 )
         )

type FCTransform = X.FormattedCell -> X.FormattedCell

infixl 5 <|
(<|) :: FCTransform -> FormatCell -> FormatCell
f <| fc = \coords idx cd -> f $ fc coords idx cd

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

-- | A union of some Cell components
data CellData
  = CellFormula X.CellFormula
  | CellValue X.CellValue
  | CellComment X.Comment

-- | Convert some Cell component into a cell
dataCell :: CellData -> X.Cell
dataCell cd =
  X.def
    & case cd of
      CellValue d -> X.cellValue ?~ d
      CellFormula d -> X.cellFormula ?~ d
      CellComment d -> X.cellComment ?~ d

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