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
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

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
  placeAtN,
  placeAt1,
  placeAt,
  placeN,
  place1,
  place,
  moveTo,
  moveBy,

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
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState, StateT (StateT), evalStateT, get, gets, modify, void)
import Control.Monad.Trans.Writer (execWriter, runWriter)
import Control.Monad.Writer (MonadWriter (..), Writer)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (Default (..))
import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map (Map, insert)
import Data.Maybe (isJust, maybeToList)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (%~), (&), (+~), (.~), (?~), (^.))

-- TODO Allow sheet addresses

-- TODO Make formulas aware of the current sheet
-- Or, make just formula printers aware so that they don't print the full address
-- when referring to data on the same sheet

-- TODO add move sheet

{- FOURMOLU_DISABLE -}
-- $Coords
{- FOURMOLU_ENABLE -}

-- | Coords of a cell
data Coords = Coords
  { _col :: Int
  , _row :: Int
  }
  deriving (Generic, Default)

-- | Make 'Coords' from a column index and a row index
mkCoords :: Int -> Int -> Coords
mkCoords _col _row = Coords{..}

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

instance Show Coords where
  show :: Coords -> String
  show (Coords{..}) = toLetters _col <> show _row

instance Num Coords where
  (+) :: Coords -> Coords -> Coords
  (+) Coords{_row = r1, _col = c1} Coords{_row = r2, _col = c2} = Coords{_row = r1 + r2, _col = c1 + c2}
  (*) :: Coords -> Coords -> Coords
  (*) Coords{_row = r1, _col = c1} Coords{_row = r2, _col = c2} = Coords{_row = r1 * r2, _col = c1 * c2}
  (-) :: Coords -> Coords -> Coords
  (-) Coords{_row = r1, _col = c1} Coords{_row = r2, _col = c2} = Coords{_row = r1 - r2, _col = c1 - c2}
  abs :: Coords -> Coords
  abs Coords{..} = Coords{_row = abs _row, _col = abs _col}
  signum :: Coords -> Coords
  signum Coords{..} = Coords{_row = signum _row, _col = signum _col}
  fromInteger :: Integer -> Coords
  fromInteger x = Coords{_row = fromIntegral (abs x), _col = fromIntegral (abs x)}

-- | Letters that can be used in column indices
alphabet :: String
alphabet = ['A' .. 'Z']

-- | Translate a number into a column letters
--
-- @
-- >>> toLetters <$> [1, 26, 27, 52, 78]
-- ["A","Z","AA","AZ","BZ"]
--
-- @
toLetters :: Int -> String
toLetters x = f "" (x - 1)
 where
  new :: Int -> String -> String
  new cur acc = [alphabet !! (cur `mod` 26)] <> acc
  f :: String -> Int -> String
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
-- >>>str = Ref (Coords 1 1) :: Ref String
-- >>>str .+ str
-- No instance for (Num String) arising from a use of `.+'
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
  setter (toCoords -> Coords{_row, _col}) f = fromCoords $ Coords{_row = f, _col}

-- | A lens for @col@s
col :: IsCoords a => Lens' a Int
col = lens getter setter
 where
  getter (toCoords -> Coords{_col}) = _col
  setter (toCoords -> Coords{_row, _col}) f = fromCoords $ Coords{_row, _col = f}

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
type FormatCell = forall a b. (ToCoords a, ToCellData b) => a -> InputIndex -> b -> X.FormattedCell

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
mkColor color _ _ (toCellData -> cd) =
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
blank _ _ (toCellData -> cd) = X.def & X.formattedCell .~ dataCell cd

-- | Transform of a formatted cell
type FCTransform = X.FormattedCell -> X.FormattedCell

-- | Apply 'FCTransform' to a 'FormatCell' to get a new 'FormatCell'
(.&) :: FormatCell -> FCTransform -> FormatCell
fc .& ft = \coords_ index cd -> ft $ fc coords_ index cd

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

type WorkSheetId = Int

type WorkBookId = Int

data BuilderState = BuilderState
  { _coords :: Coords
  , _workSheetId :: WorkSheetId
  , _workBookId :: WorkBookId
  }
  deriving (Generic, Default)

coords :: Lens' BuilderState Coords
coords = lens getter setter
 where
  getter BuilderState{..} = _coords
  setter x _coords = x{_coords}

workSheetId :: Lens' BuilderState WorkSheetId
workSheetId = lens getter setter
 where
  getter BuilderState{..} = _workSheetId
  setter x _workSheetId = x{_workSheetId}

-- | Allows to describe how to build a template for a row
newtype RowIO input output a = RowIO
  {unBuilder :: StateT BuilderState (Writer (Template input output)) a}
  deriving newtype (Functor, Applicative, Monad, MonadState BuilderState, MonadWriter (Template input output))

-- | The output is of no interest
type RowI input a = RowIO input CellData a

-- | The input and output are of no interest
type Row a = RowIO () CellData a

-- | Run builder on given coordinates. Get a result and a template
runBuilder :: RowIO input output a -> BuilderState -> (a, Template input output)
runBuilder builder coord = runWriter (evalStateT (unBuilder builder) coord)

-- | Run builder on given coordinates. Get a template
evalBuilder :: RowIO input output a -> BuilderState -> Template input output
evalBuilder builder coord = snd $ runBuilder builder coord

-- | Run builder on given coordinates. Get a result
execBuilder :: RowIO input output a -> BuilderState -> a
execBuilder builder coord = fst $ runBuilder builder coord

type RenderTemplate m input output = (Monad m, ToCellData output) => BuilderState -> InputIndex -> input -> Template input output -> m Transform
type RenderBuilderInputs m input output a = (Monad m, ToCellData output) => RowIO input output a -> [input] -> m (Transform, a)

-- | Render a builder with given coords and inputs. Return the result calculated using the topmost row
renderBuilderInputs :: (Monad m, ToCellData output) => BuilderState -> RenderTemplate m input output -> RenderBuilderInputs m input output a
renderBuilderInputs state render builder inputs = ret
 where
  ts =
    [ (newState, template)
    | inputRow <- [0 .. length inputs - 1]
    , let newState = state & coords . row +~ inputRow
          template = evalBuilder builder newState
    ]
  -- result obtained from the top row
  a = execBuilder builder state
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
  ret = (,a) <$> transform

instance FromCoords (X.RowIndex, X.ColumnIndex) where
  fromCoords :: Coords -> (X.RowIndex, X.ColumnIndex)
  fromCoords Coords{..} = (fromIntegral _row, fromIntegral _col)

-- | Render a template with a given offset, input index and input
renderTemplate :: RenderTemplate m input output
renderTemplate state inputIndex input (Template columns) = pure $ fold ps
 where
  ps =
    zipWith
      ( \columnIndex cellTemplate ->
          let
            CellTemplate{..} = cellTemplate
            leftCell = state ^. coords
            cellData = toCellData (mkOutput input)
            cellCol = leftCell ^. col + columnIndex
            cellCoords = mkCoords (leftCell ^. row) cellCol
            cell = fmtCell cellCoords inputIndex cellData
            fmTransform = Map.insert (fromCoords $ mkCoords cellCol (leftCell ^. row)) cell
            wsTransform
              -- add column width only once
              -- new properties precede old properties
              | inputIndex == 0 = X.wsColumnsProperties %~ (maybeToList columnsProperties ++)
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

-- | A column with a maybe given width and a given cell format. Return a cell reference
columnWidthFormatRef :: forall a input output. Maybe Double -> FormatCell -> (input -> output) -> RowIO input output (Ref a)
columnWidthFormatRef width fmtCell mkOutput = do
  state <- get
  let columnsProperties =
        def
          Just
          $ (unColumnsProperties def)
            { X.cpMin = state ^. coords . col
            , X.cpMax = state ^. coords . col
            , X.cpWidth = width
            }
  tell (Template [CellTemplate{fmtCell, mkOutput, columnsProperties}])
  cell <- gets (Ref . (^. coords))
  modify (coords . col +~ 1)
  pure cell

-- | A column with a given width and cell format. Returns a cell reference
columnWidthRef :: ToCellData output => Double -> FormatCell -> (input -> output) -> RowIO input CellData (Ref a)
columnWidthRef width fmtCell mkOutput = columnWidthFormatRef (Just width) fmtCell (toCellData . mkOutput)

-- | A column with a given width and cell format
columnWidth :: ToCellData output => Double -> FormatCell -> (input -> output) -> RowIO input CellData ()
columnWidth width fmtCell mkOutput = void (columnWidthRef width fmtCell mkOutput)

-- | A column with a given cell format. Returns a cell reference
columnRef :: ToCellData output => FormatCell -> (input -> output) -> RowIO input CellData (Ref a)
columnRef fmtCell mkOutput = columnWidthFormatRef Nothing fmtCell (toCellData . mkOutput)

-- | A column with a given cell format
column :: ToCellData output => FormatCell -> (input -> output) -> RowIO input CellData ()
column fmtCell mkOutput = void (columnRef fmtCell mkOutput)

-- | Produce a transform and a result from a template renderer, inputs, and a builder
composeTransformAndResult :: forall a input output. ToCellData output => RenderTemplate Identity input output -> BuilderState -> [input] -> RowIO input output a -> (Transform, a)
composeTransformAndResult renderTemplate' offset input builder = runIdentity $ renderBuilderInputs offset renderTemplate' builder input

-- | Produce a result from a default template renderer, inputs, and a builder
defaultComposeTransformWithResult :: ToCellData output => BuilderState -> [input] -> RowIO input output a -> (Transform, a)
defaultComposeTransformWithResult = composeTransformAndResult renderTemplate

{- FOURMOLU_DISABLE -}
-- $Sheet
{- FOURMOLU_ENABLE -}

-- | A builder to compose the results of 'Transform's
newtype Sheet a = Sheet {unSheet :: StateT BuilderState (Writer Transform) a}
  deriving newtype (Functor, Applicative, Monad, MonadWriter Transform, MonadState BuilderState)

-- | Starting at a given coordinate, place a list of inputs according to a row builder and return a result
placeAtN :: (ToCellData output, ToCoords c) => c -> [input] -> RowIO input output a -> Sheet a
placeAtN (toCoords -> coords_) inputs b = do
  state <- get
  let transformResult = defaultComposeTransformWithResult (state & coords .~ coords_) inputs b
  tell (fst transformResult)
  pure (snd transformResult)

-- | Starting at a given coordinate, place one input according to a row builder and return a result
placeAt1 :: (ToCellData output, ToCoords c) => c -> input -> RowIO input output a -> Sheet a
placeAt1 coords_ input = placeAtN coords_ [input]

-- | Starting at a given coordinate, place a row builder and return a result
placeAt :: (ToCellData output, ToCoords c) => c -> RowIO () output a -> Sheet a
placeAt coords_ = placeAt1 coords_ ()

-- | Set the current coordinate in a 'Sheet'
moveTo :: ToCoords c => c -> Sheet a -> Sheet a
moveTo c t = modify (\x -> x & coords .~ toCoords c) >> t

-- | Shift the current coordinate in a 'Sheet'
moveBy :: ToCoords c => c -> Sheet a -> Sheet a
moveBy c t = modify (\x -> x & coords +~ toCoords c) >> t

-- | Starting at the current coordinate in the 'Sheet', place a list of inputs according to a row builder and return a result
placeN :: ToCellData output => [input] -> RowIO input output a -> Sheet a
placeN input b = get >>= \state -> placeAtN (state ^. coords) input b

-- | Starting at the current coordinate in the 'Sheet', place a single input according to a row builder and return a result
place1 :: ToCellData output => input -> RowIO input output a -> Sheet a
place1 input = placeN [input]

-- | Starting at the current coordinate in the 'Sheet', place a row builder and return a result
place :: ToCellData output => RowIO () output a -> Sheet a
place = place1 ()

{- FOURMOLU_DISABLE -}
-- $Expressions
{- FOURMOLU_ENABLE -}

-- | Expressions
data Expr t
  = EBinOp BinaryOperator (Expr t) (Expr t)
  | EFunction String [Expr t]
  | ERef (Ref t)
  | ERange (Ref t) (Ref t)

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

-- | Formula
newtype Formula t = Formula {unFormula :: Expr t}
  deriving newtype (UnsafeChangeType, Show)

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

showOp2 :: (Show a, Show b) => String -> a -> b -> String
showOp2 operator c1 c2 = show c1 <> operator <> show c2

mkOp2 :: (ToFormula a, ToFormula b) => BinaryOperator -> a -> b -> Formula t
mkOp2 f c1 c2 = Formula $ EBinOp f (unFormula $ toFormula c1) (unFormula $ toFormula c2)

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
mkBoolOp2 f c1 c2 = Formula $ EBinOp f (unFormula $ toFormula c1) (unFormula $ toFormula c2)

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

instance Show (Expr t) where
  show :: Expr t -> String
  show (EBinOp OpAdd c1 c2) = showOp2 "+" c1 c2
  show (EBinOp OpSubtract c1 c2) = showOp2 "-" c1 c2
  show (EBinOp OpMultiply c1 c2) = showOp2 "*" c1 c2
  show (EBinOp OpDivide c1 c2) = showOp2 "/" c1 c2
  show (EBinOp OpPower c1 c2) = showOp2 "^" c1 c2
  show (EBinOp OpLT c1 c2) = showOp2 "<" c1 c2
  show (EBinOp OpGT c1 c2) = showOp2 ">" c1 c2
  show (EBinOp OpLEQ c1 c2) = showOp2 "<=" c1 c2
  show (EBinOp OpGEQ c1 c2) = showOp2 ">=" c1 c2
  show (EBinOp OpEQ c1 c2) = showOp2 "=" c1 c2
  show (EBinOp OpNEQ c1 c2) = showOp2 "<>" c1 c2
  show (ERef (Ref e)) = show e
  show (ERange (Ref c1) (Ref c2)) = show c1 ++ ":" ++ show c2
  show (EFunction n as) = n <> "(" <> intercalate "," (show <$> as) <> ")"

instance UnsafeChangeType Expr where
  unsafeChangeType :: Expr b -> Expr c
  unsafeChangeType (EBinOp a b c) = EBinOp a (unsafeChangeType b) (unsafeChangeType c)
  unsafeChangeType (ERef (Ref a)) = ERef (Ref a)
  unsafeChangeType (EFunction n as) = EFunction n (unsafeChangeType <$> as)
  unsafeChangeType (ERange l r) = ERange (unsafeChangeType l) (unsafeChangeType r)

-- | Name of a function like @SUM@
type FunName = String

class MakeFunction t where
  makeFunction :: FunName -> [Formula s] -> t

instance MakeFunction (Formula a) where
  makeFunction :: FunName -> [Formula s] -> Formula a
  makeFunction name args = Formula $ EFunction name (unsafeChangeType . unFormula . toFormula <$> args)

instance (Foldable f, MakeFunction t, ToFormula a) => MakeFunction (f a -> t) where
  makeFunction :: (Foldable f, MakeFunction t, ToFormula a) => FunName -> [Formula s] -> f a -> t
  makeFunction name args xs = makeFunction name ((unsafeChangeType . toFormula <$> args) ++ foldMap ((: []) . unsafeChangeType . toFormula) xs)

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

instance ToCellData (Formula a) where
  toCellData :: Formula a -> CellData
  toCellData (Formula e) = toCellData e

{- FOURMOLU_DISABLE -}
-- $Xlsx
{- FOURMOLU_ENABLE -}

-- | Compose an @xlsx@ from a list of sheet names and builders
composeXlsx :: [(T.Text, Sheet ())] -> X.Xlsx
composeXlsx sheetBuilders = workBook'
 where
  getTransform index x = execWriter $ flip evalStateT (def & workSheetId .~ index) $ unSheet x
  workBook =
    flip X.formatWorkbook X.def $
      zipWith
        (\index (name, tf) -> (name, (fmTransform $ getTransform index tf) X.def))
        [1 ..]
        sheetBuilders
  filterWidths ws = ws & X.wsColumnsProperties %~ filter (isJust . X.cpWidth)
  workBook' =
    workBook
      & X.xlSheets
        %~ \sheets ->
          zipWith3
            (\index x (name, ws) -> (name, (getTransform index x & wsTransform) ws & filterWidths))
            [0 ..]
            (snd <$> sheetBuilders)
            sheets

-- | Lazily write an xlsx
writeXlsx :: FilePath -> [(T.Text, Sheet ())] -> IO ()
writeXlsx file sheets = do
  ct <- getPOSIXTime
  let xlsx = composeXlsx sheets
  LBS.writeFile file $ X.fromXlsx ct xlsx
