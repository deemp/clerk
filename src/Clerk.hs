{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

{-# HLINT ignore "Redundant bracket" #-}

-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | @Clerk@ library
module Clerk (
  -- * Coords
  -- $Coords
  Coords,
  mkCoords,
  ToCoords (..),
  FromCoords (..),
  IsCoords,

  -- * Cell references
  -- $Ref
  Ref,
  row,
  col,
  UnsafeChangeType (..),

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
  Row,
  RowI,
  RowIO,
  Template,

  -- * Columns
  -- $Columns
  ColumnsProperties,
  columnWidthFormatRef,
  columnWidthRef,
  columnWidth,
  columnRef,
  column,

  -- * Sheet builder
  -- $Sheet
  Sheet,
  placeN,
  place1,
  place,
  evalSheetDefault,
  -- TODO make this module internal
  -- move to ForExamples
  SheetState (..),
  Sheet (..),
  Coords (..),
  RowState (..),
  RowShow (..),
  evalRow,

  -- * Expressions
  -- $Expressions
  Expr,
  Formula,
  ToFormula (..),
  NumOperator,
  (.+),
  (.-),
  (.*),
  (./),
  (.:),
  (.^),
  (.<),
  (.>),
  (.<=),
  (.>=),
  (.=),
  (.<>),
  (.&),
  fun,
  FunName,

  -- * Cells
  -- $Cells
  CellData,
  ToCellData (..),

  -- * xlsx
  -- $Xlsx
  composeXlsx,
  writeXlsx,
) where

import Codec.Xlsx qualified as X
import Codec.Xlsx.Formatted qualified as X
import Control.Monad (forM, void, zipWithM)
import Control.Monad.State (MonadState, StateT (StateT), evalStateT, get, gets, modify)
import Control.Monad.Trans.Writer (execWriter, runWriter)
import Control.Monad.Writer (MonadWriter (..), Writer)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (Default (..))
import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map (Map, insert)
import Data.Maybe (isJust, maybeToList)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (%~), (&), (+~), (.~), (?~), (^.))
import Unsafe.Coerce (unsafeCoerce)

-- TODO add modes to state
-- Google Sheets, Excel, Tabular

-- TODO Formula example

-- TODO multiple sheets example

{- FOURMOLU_DISABLE -}
-- $Coords
{- FOURMOLU_ENABLE -}

-- | Coords of a cell
data Coords = Coords
  { _col :: Int
  , _row :: Int
  , _coordsWorksheetName :: T.Text
  , _coordsWorkbookPath :: FilePath
  }
  deriving (Generic, Default)

instance Default T.Text where
  def :: T.Text
  def = ""

-- | Make 'Coords' from a column index and a row index
mkCoords :: Int -> Int -> Sheet Coords
mkCoords _col _row = do
  SheetState{_sheetWorkbookPath = _coordsWorkbookPath, _sheetWorksheetName = _coordsWorksheetName} <- get
  pure Coords{_col, _row, ..}

-- | Convertible to 'Coords'
class ToCoords a where
  toCoords :: a -> Coords

-- | Convertible from 'Coords'
class FromCoords a where
  fromCoords :: Coords -> a

type IsCoords a = (FromCoords a, ToCoords a)

instance ToCoords Coords where
  toCoords :: Coords -> Coords
  toCoords = id

instance FromCoords Coords where
  fromCoords :: Coords -> Coords
  fromCoords = id

class RowShow a where
  rowShow :: a -> Row T.Text

instance RowShow Coords where
  rowShow :: Coords -> Row T.Text
  rowShow cs = do
    state <- get
    let
      prefix
        | (cs & _coordsWorkbookPath) /= (state & _coordsWorkbookPath) =
            ("'[" <> T.pack (cs & _coordsWorkbookPath) <> "]" <> (cs & _coordsWorksheetName) <> "'!")
        | (cs & _coordsWorksheetName) /= (state & _coordsWorksheetName) = (cs & _coordsWorksheetName) <> "!"
        | otherwise = ""
    pure $ prefix <> toLetters (cs ^. col) <> T.pack (show (cs ^. row))

-- instance Show a => RowShow a where
--   rowShow a =

-- instance Show Coords where
--   show :: Coords -> T.Text
--   show (Coords{..}) = "[Book" <> show _coordsWorkbookId <> "]" <> toLetters _col <> show _row

instance Num Coords where
  (+) :: Coords -> Coords -> Coords
  (+) Coords{_row = r1, _col = c1, ..} Coords{_row = r2, _col = c2} = Coords{_row = r1 + r2, _col = c1 + c2, ..}
  (*) :: Coords -> Coords -> Coords
  (*) Coords{_row = r1, _col = c1, ..} Coords{_row = r2, _col = c2} = Coords{_row = r1 * r2, _col = c1 * c2, ..}
  (-) :: Coords -> Coords -> Coords
  (-) Coords{_row = r1, _col = c1, ..} Coords{_row = r2, _col = c2} = Coords{_row = r1 - r2, _col = c1 - c2, ..}
  abs :: Coords -> Coords
  abs Coords{..} = Coords{_row = abs _row, _col = abs _col, ..}
  signum :: Coords -> Coords
  signum Coords{..} = Coords{_row = signum _row, _col = signum _col, ..}

  -- shouldn't be used in lenses as it sets the default values
  fromInteger :: Integer -> Coords
  fromInteger x = def{_row = fromIntegral (abs x), _col = fromIntegral (abs x)}

-- | Letters that can be used in column indices
alphabet :: [Char]
alphabet = ['A' .. 'Z']

-- | Translate a number into a column letters
--
-- @
-- >>> toLetters <$> [1, 26, 27, 52, 78]
-- ["A","Z","AA","AZ","BZ"]
--
-- @
toLetters :: Int -> T.Text
toLetters x = f "" (x - 1)
 where
  new :: Int -> T.Text -> T.Text
  new cur acc = T.pack [alphabet !! (cur `mod` 26)] <> acc
  f :: T.Text -> Int -> T.Text
  f acc cur = if cur `div` 26 > 0 then f (new cur acc) (cur `div` 26 - 1) else new cur acc

{- FOURMOLU_DISABLE -}
-- $Ref
{- FOURMOLU_ENABLE -}

-- | A typed reference to a cell.
--
-- The user is responsible for setting the necessary cell type.
--
-- The type prevents operations between cell references with incompatible types.
--
-- @
-- >>>str = Ref (Coords 1 1) :: Ref T.Text
-- >>>str .+ str
-- No instance for (Num T.Text) arising from a use of `.+'
-- In the expression: str .+ str
-- In an equation for `it_a18COt': it_a18COt = str .+ str
--
-- @
-- When necessary, one can change the cell reference type via 'unsafeChangeRefType'
--
-- @
-- >>>int = Ref (Coords 1 1) :: Ref Int
-- >>>double = Ref (Coords 2 5) :: Ref Double
-- >>>unsafeChangeType int .+ double
-- A1+E2
--
-- @
newtype Ref a = Ref {unRef :: Coords}
  deriving (Num) via Coords

instance ToCoords (Ref a) where
  toCoords :: Ref a -> Coords
  toCoords = unRef

instance FromCoords (Ref a) where
  fromCoords :: Coords -> Ref a
  fromCoords = Ref

-- | A lens for @row@s
row :: IsCoords a => Lens' a Int
row = lens getter setter
 where
  getter (toCoords -> Coords{_row}) = _row
  setter (toCoords -> Coords{_row, _col, ..}) f = fromCoords $ Coords{_row = f, _col, ..}

-- | A lens for @col@s
col :: IsCoords a => Lens' a Int
col = lens getter setter
 where
  getter (toCoords -> Coords{_col}) = _col
  setter (toCoords -> Coords{_row, _col, ..}) f = fromCoords $ Coords{_row, _col = f, ..}

-- | Change the type of something. Use with caution!
class UnsafeChangeType (a :: Type -> Type) where
  unsafeChangeType :: forall c b. a b -> a c

instance UnsafeChangeType Ref where
  unsafeChangeType :: Ref b -> Ref c
  unsafeChangeType (Ref c) = Ref c

{- FOURMOLU_DISABLE -}
-- $Formatting
{- FOURMOLU_ENABLE -}

-- | Index of an input
type InputIndex = Int

-- | Format a single cell depending on its coordinates, index, and data
type FormatCell = forall a b. (ToCoords a, ToCellData b) => a -> InputIndex -> b -> Row X.FormattedCell

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
mkColor color _ _ c = do
  cd <- toCellData c
  pure $
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
blank _ _ cd_ = do
  cd <- toCellData cd_
  do pure $ X.def & X.formattedCell .~ dataCell cd

-- | Transform of a formatted cell
type FCTransform = X.FormattedCell -> X.FormattedCell

-- | Apply 'FCTransform' to a 'FormatCell' to get a new 'FormatCell'
(.&) :: FormatCell -> FCTransform -> FormatCell
fc .& ft = \coords_ index cd -> ft <$> fc coords_ index cd

infixl 5 .&

-- | Get a 'FCTransform' with a given horizontal alignment in a cell
horizontalAlignment :: X.CellHorizontalAlignment -> FCTransform
horizontalAlignment alignment fc =
  fc
    & X.formattedFormat
      %~ X.formatAlignment
      ?~ (X.def & X.alignmentHorizontal ?~ alignment)

{- FOURMOLU_DISABLE -}
-- $Templates
{- FOURMOLU_ENABLE -}

-- | Template for multiple cells
newtype Template input output = Template [CellTemplate input output]
  deriving newtype (Semigroup, Monoid)

type RowState = Coords

-- | A monad for describing a horizontal block of data - a @row@
newtype RowIO input output a = Row
  {unRow :: StateT RowState (Writer (Template input output)) a}
  deriving newtype (Functor, Applicative, Monad, MonadState RowState, MonadWriter (Template input output))

-- | Row with a default 'CellData' output
type RowI input a = RowIO input CellData a

-- | Row with a default @()@ input
type RowO output a = RowIO () output a

-- | Row with a default @()@ input and a default 'CellData' output
type Row a = RowO CellData a

-- | Run builder on given coordinates. Get a result and a template
runBuilder :: RowIO input output a -> RowState -> (a, Template input output)
runBuilder builder coord = runWriter (evalStateT (unRow builder) coord)

-- | Run builder on given coordinates. Get a template
execRow :: RowIO input output a -> RowState -> Template input output
execRow builder state = snd $ runBuilder builder state

-- | Run builder on given coordinates. Get a result
evalRow :: RowIO input output a -> RowState -> a
evalRow builder state = fst $ runBuilder builder state

type RenderTemplate m input output = (Monad m, ToCellData output) => RowState -> InputIndex -> input -> Template input output -> Sheet Transform
type RenderInputs m input output a = (Monad m, ToCellData output) => [input] -> RowIO input output a -> Sheet (Transform, a)

-- | Render inputs starting at given coords and using a row. Return the result calculated using the topmost row
renderInputs :: ToCellData output => RowState -> RenderTemplate Sheet input output -> RenderInputs Sheet input output a
renderInputs state render inputs row_ = do
  let
    ts =
      [ (newState, template)
      | inputRow <- [0 .. length inputs - 1]
      , let newState = state & row +~ inputRow
            template = execRow row_ newState
      ]
    -- result obtained from the top row
    rowResult = evalRow row_ state
    transform =
      fold
        <$> sequenceA
          ( zipWith3
              ( \input inputIndex (st, template) ->
                  render st inputIndex input template
              )
              inputs
              [0 ..]
              ts
          )
  (,rowResult) <$> transform

instance FromCoords (X.RowIndex, X.ColumnIndex) where
  fromCoords :: Coords -> (X.RowIndex, X.ColumnIndex)
  fromCoords Coords{..} = (fromIntegral _row, fromIntegral _col)

-- | Render a template with a given offset, input index and input
renderTemplate :: RenderTemplate Sheet input output
renderTemplate state inputIndex input (Template columns) = do
  ps <-
    zipWithM
      ( \columnIndex cellTemplate -> do
          let
            CellTemplate{..} = cellTemplate
            leftCell = state
            cellCol = leftCell ^. col + columnIndex
          cellCoords <- mkCoords (leftCell ^. row) cellCol
          let cellData = fst $ runWriter $ flip evalStateT cellCoords $ unRow $ toCellData (mkOutput input)
              cell = fst $ runWriter $ flip evalStateT cellCoords $ unRow $ fmtCell cellCoords inputIndex cellData
          let wsTransform
                -- add column width only once
                -- new properties precede old properties
                | inputIndex == 0 = X.wsColumnsProperties %~ (maybeToList columnsProperties ++)
                | otherwise = id
          newCoords <- mkCoords cellCol (leftCell ^. row)
          let fmTransform = Map.insert (fromCoords newCoords) cell
          pure def{fmTransform, wsTransform}
      )
      [0 ..]
      columns
  pure $ fold ps

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

-- | A column with a maybe given width and a given cell format. Return a cell reference
columnWidthFormatRef :: forall a input output. Maybe Double -> FormatCell -> (input -> output) -> RowIO input output (Ref a)
columnWidthFormatRef width fmtCell mkOutput = do
  state <- get
  let columnsProperties =
        Just $
          (unColumnsProperties def)
            { X.cpMin = state ^. col
            , X.cpMax = state ^. col
            , X.cpWidth = width
            }
  tell (Template [CellTemplate{fmtCell, mkOutput, columnsProperties}])
  cell <- gets Ref
  modify (col +~ 1)
  pure cell

-- | A column with a given width and cell format. Returns a cell reference
columnWidthRef :: ToCellData output => Double -> FormatCell -> (input -> output) -> RowI input (Ref a)
columnWidthRef width fmtCell mkOutput = do
  state <- get
  columnWidthFormatRef (Just width) fmtCell (fst . runWriter . flip evalStateT state . unRow . toCellData . mkOutput)

-- | A column with a given width and cell format
columnWidth :: ToCellData output => Double -> FormatCell -> (input -> output) -> RowI input ()
columnWidth width fmtCell mkOutput = void (columnWidthRef width fmtCell mkOutput)

-- | A column with a given cell format. Returns a cell reference
columnRef :: ToCellData output => FormatCell -> (input -> output) -> RowI input (Ref a)
columnRef fmtCell mkOutput = do
  state <- get
  columnWidthFormatRef Nothing fmtCell (fst . runWriter . flip evalStateT state . unRow . toCellData . mkOutput)

-- | A column with a given cell format
column :: ToCellData output => FormatCell -> (input -> output) -> RowI input ()
column fmtCell mkOutput = void (columnRef fmtCell mkOutput)

{- FOURMOLU_DISABLE -}
-- $Sheet
{- FOURMOLU_ENABLE -}

data SheetState = SheetState
  { _sheetWorksheetName :: T.Text
  , _sheetWorkbookPath :: FilePath
  }

-- | A builder to compose the results of 'Transform's
newtype Sheet a = Sheet {unSheet :: StateT SheetState (Writer Transform) a}
  deriving newtype (Functor, Applicative, Monad, MonadWriter Transform, MonadState SheetState)

-- | Evaluate the result of a sheet with a default state
evalSheetDefault :: Sheet a -> a
evalSheetDefault s = fst $ runWriter $ flip evalStateT (SheetState{_sheetWorksheetName = "worksheet", _sheetWorkbookPath = "workbook"}) $ unSheet s

-- | Starting at a given coordinate, place a list of inputs according to a row builder and return a result
placeN :: (ToCellData output, ToCoords c) => c -> [input] -> RowIO input output a -> Sheet a
placeN (toCoords -> state) inputs b = do
  transformResult <- renderInputs state renderTemplate inputs b
  tell (fst transformResult)
  pure (snd transformResult)

-- | Starting at a given coordinate, place one input according to a row builder and return a result
place1 :: (ToCellData output, ToCoords c) => c -> input -> RowIO input output a -> Sheet a
place1 coords_ input = placeN coords_ [input]

-- | Starting at a given coordinate, place a row builder and return a result
place :: (ToCellData output, ToCoords c) => c -> RowO output a -> Sheet a
place coords_ = place1 coords_ ()

{- FOURMOLU_DISABLE -}
-- $Expressions
{- FOURMOLU_ENABLE -}

-- | Expressions
data Expr t
  = EBinaryOp {binOp :: BinaryOperator, arg1 :: (Expr t), arg2 :: (Expr t)}
  | EFunction {fName :: T.Text, fArgs :: [Expr t]}
  | ERef {ref :: (Ref t)}
  | ERange {ref1 :: (Ref t), ref2 :: (Ref t)}
  | EValue {value :: t}
  | EUnaryOp {unaryOp :: UnaryOp, arg :: Expr t}

data BinaryOperator
  = OpAdd
  | OpSubtract
  | OpMultiply
  | OpDivide
  | OpPower
  | OpLT
  | OpGT
  | OpLEQ
  | OpGEQ
  | OpEQ
  | OpNEQ

data UnaryOp
  = OpNeg

-- | Formula
newtype Formula t = Formula {unFormula :: Expr t}
  deriving newtype (UnsafeChangeType, RowShow)

-- | Something that can be turned into a formula
class ToFormula a where
  toFormula :: a -> Formula t

instance ToFormula (Ref a) where
  toFormula :: Ref a -> Formula t
  toFormula (Ref c) = Formula $ ERef (Ref c)

instance ToFormula Coords where
  toFormula :: Coords -> Formula t
  toFormula c = Formula $ ERef (Ref c)

instance ToFormula (Expr a) where
  toFormula :: Expr a -> Formula b
  toFormula = Formula . unsafeChangeType

instance ToFormula (Formula a) where
  toFormula :: Formula a -> Formula b
  toFormula (Formula f) = Formula $ unsafeChangeType f

-- TODO dangerous?
instance {-# OVERLAPPABLE #-} Num a => ToFormula a where
  toFormula :: a -> Formula b
  toFormula = Formula . EValue . unsafeCoerce

showOp2 :: (RowShow a, RowShow b) => a -> b -> T.Text -> Row T.Text
showOp2 c1 c2 operator = do
  d1 <- rowShow c1
  d2 <- rowShow c2
  pure $ d1 <> operator <> d2

mkOp2 :: (ToFormula a, ToFormula b) => BinaryOperator -> a -> b -> Formula t
mkOp2 f c1 c2 = Formula $ EBinaryOp f (unFormula $ toFormula c1) (unFormula $ toFormula c2)

mkNumOp2 :: (Num t, ToFormula a, ToFormula b) => BinaryOperator -> a -> b -> Formula t
mkNumOp2 = mkOp2

-- | Construct a range expression
(.:) :: forall c a b. Ref a -> Ref b -> Formula c
(.:) a b = Formula $ ERange (unsafeChangeType a) (unsafeChangeType b)

infixr 5 .:

-- | A type for numeric operators
type NumOperator a b c = (Num a, ToFormula (b a), ToFormula (c a)) => b a -> c a -> Formula a

-- | Construct an addition expression like @A1 + B1@
(.+) :: NumOperator a b c
(.+) = mkNumOp2 OpAdd

infixl 6 .+

-- | Construct a subtraction expression like @A1 - B1@
(.-) :: NumOperator a b c
(.-) = mkNumOp2 OpSubtract

infixl 6 .-

-- | Construct a division expression like @A1 / B1@
(./) :: NumOperator a b c
(./) = mkNumOp2 OpDivide

infixl 7 ./

-- | Construct a multiplication expression like @A1 * B1@
(.*) :: NumOperator a b c
(.*) = mkNumOp2 OpMultiply

infixl 6 .*

-- | Construct an exponentiation expression like @A1 ^ B1@
(.^) :: NumOperator a b c
(.^) = mkNumOp2 OpPower

infixr 8 .^

type BoolOperator a b c = (Ord a, ToFormula (b a), ToFormula (c a)) => b a -> c a -> Formula Bool

mkBoolOp2 :: (Ord a, ToFormula (b a), ToFormula (c a)) => BinaryOperator -> b a -> c a -> Formula Bool
mkBoolOp2 f c1 c2 = Formula $ EBinaryOp f (unFormula $ toFormula c1) (unFormula $ toFormula c2)

-- | Construct a @less-than@ expression like @A1 < B1@
(.<) :: BoolOperator a b c
(.<) = mkBoolOp2 OpLT

infix 4 .<

-- | Construct a @greater-than@ expression like @A1 > B1@
(.>) :: BoolOperator a b c
(.>) = mkBoolOp2 OpGT

infix 4 .>

-- | Construct a @less-than-or-equal-to@ expression like @A1 <= B1@
(.<=) :: BoolOperator a b c
(.<=) = mkBoolOp2 OpLEQ

infix 4 .<=

-- | Construct a @greater-than-or-equal-to@ expression like @A1 <= B1@
(.>=) :: BoolOperator a b c
(.>=) = mkBoolOp2 OpGEQ

infix 4 .>=

-- | Construct a @equal-to@ expression like @A1 = B1@
(.=) :: BoolOperator a b c
(.=) = mkBoolOp2 OpEQ

infix 4 .=

-- | Construct a @not-equal-to@ expression like @A1 <> B1@
(.<>) :: BoolOperator a b c
(.<>) = mkBoolOp2 OpNEQ

infix 4 .<>

instance RowShow (Expr t) where
  rowShow :: Expr t -> Row T.Text
  rowShow (EBinaryOp{..}) =
    showOp2 arg1 arg2 $
      case binOp of
        OpAdd -> "+"
        OpSubtract -> "-"
        OpMultiply -> "*"
        OpDivide -> "/"
        OpPower -> "^"
        OpLT -> "<"
        OpGT -> ">"
        OpLEQ -> "<="
        OpGEQ -> ">="
        OpEQ -> "="
        OpNEQ -> "<>"
  rowShow (ERef (Ref e)) = rowShow e
  rowShow (ERange (Ref c1) (Ref c2)) = do
    d1 <- rowShow c1
    d2 <- rowShow c2
    pure $ d1 <> ":" <> d2
  rowShow (EFunction n as) = do
    d1 <- forM as rowShow
    pure $ n <> "(" <> T.intercalate "," d1 <> ")"

-- rowShow (EUnaryOp{..}) =
--   case unaryOp of
--     OpId -> rowShow arg
--     OpNeg -> rowShow arg

instance UnsafeChangeType Expr where
  unsafeChangeType :: Expr b -> Expr c
  unsafeChangeType (EBinaryOp a b c) = EBinaryOp a (unsafeChangeType b) (unsafeChangeType c)
  unsafeChangeType (ERef (Ref a)) = ERef (Ref a)
  unsafeChangeType (EFunction n as) = EFunction n (unsafeChangeType <$> as)
  unsafeChangeType (ERange l r) = ERange (unsafeChangeType l) (unsafeChangeType r)
  unsafeChangeType (EUnaryOp u v) = EUnaryOp u (unsafeCoerce v)

-- | Name of a function like @SUM@
type FunName = T.Text

class MakeFunction t where
  makeFunction :: FunName -> [Formula s] -> t

instance MakeFunction (Formula a) where
  makeFunction :: FunName -> [Formula s] -> Formula a
  makeFunction name args = Formula $ EFunction name (unsafeChangeType . unFormula . toFormula <$> args)

instance (Foldable f, MakeFunction t, ToFormula a) => MakeFunction (f a -> t) where
  makeFunction :: (Foldable f, MakeFunction t, ToFormula a) => FunName -> [Formula s] -> f a -> t
  makeFunction name args xs =
    makeFunction
      name
      ((unsafeChangeType . toFormula <$> args) ++ foldMap ((: []) . unsafeChangeType . toFormula) xs)

-- | Construct a function like @SUM(A1,B1)@
fun :: MakeFunction t => FunName -> t
fun n = makeFunction n []

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

-- | Convert some Ref component into a cell
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
  toCellData :: a -> Row CellData

instance ToCellData T.Text where
  toCellData :: T.Text -> Row CellData
  toCellData = pure . CellValue . X.CellText

instance ToCellData String where
  toCellData :: String -> Row CellData
  toCellData = pure . CellValue . X.CellText . T.pack

instance ToCellData Int where
  toCellData :: Int -> Row CellData
  toCellData = pure . CellValue . X.CellDouble . fromIntegral

instance ToCellData Double where
  toCellData :: Double -> Row CellData
  toCellData = pure . CellValue . X.CellDouble

instance ToCellData Bool where
  toCellData :: Bool -> Row CellData
  toCellData = pure . CellValue . X.CellBool

instance ToCellData CellData where
  toCellData :: CellData -> Row CellData
  toCellData = pure

instance ToCellData (Expr a) where
  toCellData :: Expr a -> Row CellData
  toCellData e_ = do
    e <- rowShow e_
    pure $
      CellFormula
        X.CellFormula
          { X._cellfAssignsToName = False
          , X._cellfCalculate = True
          , X._cellfExpression = X.NormalFormula $ X.Formula e
          }

instance ToCellData (Formula a) where
  toCellData :: Formula a -> Row CellData
  toCellData (Formula e) = toCellData e

{- FOURMOLU_DISABLE -}
-- $Xlsx
{- FOURMOLU_ENABLE -}

-- | Compose an @xlsx@ from a list of sheet names and builders
composeXlsx :: FilePath -> [(T.Text, Sheet ())] -> X.Xlsx
composeXlsx path sheetBuilders = workBookWithColumnWidths
 where
  getTransform _sheetWorksheetName x =
    execWriter
      $ flip
        evalStateT
        (SheetState{_sheetWorkbookPath = path, ..})
      $ unSheet x
  workBookWithData =
    flip X.formatWorkbook X.def $
      (\(name, tf) -> (name, (getTransform name tf & fmTransform) X.def))
        <$> sheetBuilders
  workBookWithColumnWidths =
    workBookWithData
      & X.xlSheets
        %~ \sheets ->
          zipWith
            ( \x (name, ws) ->
                ( name
                , ws
                    & (getTransform name x & wsTransform)
                    & X.wsColumnsProperties %~ filter (isJust . X.cpWidth)
                )
            )
            (snd <$> sheetBuilders)
            sheets

-- | Lazily write an xlsx
writeXlsx :: FilePath -> [(T.Text, Sheet ())] -> IO ()
writeXlsx file sheets = do
  ct <- getPOSIXTime
  let xlsx = composeXlsx file sheets
  LBS.writeFile file $ X.fromXlsx ct xlsx
