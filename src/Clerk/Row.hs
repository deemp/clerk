module Clerk.Row where

import Clerk.Coordinates
import Clerk.Expression
import Clerk.Formula
import Clerk.Reference
import Codec.Xlsx (RowIndex (unRowIndex))
import qualified Codec.Xlsx as X
import qualified Codec.Xlsx.Formatted as X
import Control.Monad
import Control.Monad.State (MonadState (get), StateT, evalStateT)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Default (Default (def))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro

-- Row ---------

-- | Format a single cell based on its coordinates, index in an input list, and data.
type FormatCell = forall a b. (ToCoords a, ToCellData b) => InputIndex -> a -> b -> Row X.FormattedCell

-- | Template of a cell with content, style, column properties
data CellTemplate input output = CellTemplate
  { _mkOutput :: input -> output
  , _fmtCell :: FormatCell
  , _columnsProperties :: Maybe X.ColumnsProperties
  }

-- | Template for multiple cells
newtype Template input output = Template [CellTemplate input output]
  deriving newtype (Semigroup, Monoid)

type RowState = Coords

-- | A monad for describing a horizontal block of data - a @row@
newtype RowIO input output a = RowIO
  {_rowIO :: StateT RowState (Writer (Template input output)) a}
  deriving (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadState RowState, MonadWriter (Template input output))

-- | Row with a default 'CellData' output
type RowI input a = RowIO input CellData a

-- | Row with a default @()@ input
type RowO output a = RowIO () output a

-- | Row with a default @()@ input and a default 'CellData' output
type Row a = RowO CellData a

-- | Run builder on given coordinates. Get a result and a template
runRow :: RowIO input output a -> RowState -> (a, Template input output)
runRow builder state = runWriter (evalStateT (_rowIO builder) state)

-- | Run builder on given coordinates. Get a template
execRow :: RowIO input output a -> RowState -> Template input output
execRow builder state = snd $ runRow builder state

-- | Run builder on given coordinates. Get a result
evalRow :: RowIO input output a -> RowState -> a
evalRow builder state = fst $ runRow builder state

-- RowShow ---------

-- | Show in context of a 'Row'
class Show a => RowShow a where
  rowShow :: a -> Row T.Text

instance RowShow Coords where
  rowShow :: Coords -> Row T.Text
  rowShow cs = do
    state <- get
    let
      prefix
        | (cs & _coordsWorkbookPath) /= (state & _coordsWorkbookPath) =
            "'[" <> T.pack (cs & _coordsWorkbookPath) <> "]" <> (cs & (T.pack . _coordsWorksheetName)) <> "'!"
        | (cs & _coordsWorksheetName) /= (state & _coordsWorksheetName) = (cs & (T.pack . _coordsWorksheetName)) <> "!"
        | otherwise = ""
    pure $ prefix <> toLetters (cs ^. col) <> T.pack (show (cs ^. row . to unRowIndex))

showBinaryOperation :: (RowShow a, RowShow b) => a -> b -> T.Text -> Row T.Text
showBinaryOperation arg1 arg2 operator = do
  d1 <- rowShow arg1
  d2 <- rowShow arg2
  pure $ d1 <> operator <> d2

instance Show (Expr t) => RowShow (Expr t) where
  rowShow :: Expr t -> Row T.Text
  rowShow (EBinaryOperation{..}) =
    showBinaryOperation _argLeft _argRight $
      case _binaryOperator of
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
  rowShow (EFunction n args) = do
    d1 <- forM args rowShow
    pure $ (n & _functionName) <> "(" <> T.intercalate "," d1 <> ")"
  rowShow (EUnaryOperation{..}) =
    case _unaryOperator of
      OpNeg -> rowShow _arg
  rowShow EValue{..} = pure $ T.pack $ show (EValue{..})

instance Show (Expr t) => RowShow (Formula t) where
  rowShow :: Formula t -> Row T.Text
  rowShow (Formula f) = rowShow f

-- | Show in a default 'Row' context
rowShowDefault :: RowShow a => a -> Text
rowShowDefault a = evalRow (rowShow a) def

-- CellData ---------

-- | A union of what can be inside a cell
data CellData
  = CellFormula X.CellFormula
  | CellValue X.CellValue
  | CellComment X.Comment
  | CellEmpty
  deriving stock (Show)

instance Default CellData where
  def :: CellData
  def = CellEmpty

-- | Something that can be turned into 'CellData' in a `Row` context
class Show a => ToCellData a where
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

instance ToCellData InputIndex where
  toCellData (InputIndex i) = toCellData i

instance Show (Expr a) => ToCellData (Expr a) where
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

instance (Show (Formula a), Show (Expr a)) => ToCellData (Formula a) where
  toCellData :: Formula a -> Row CellData
  toCellData (Formula e) = toCellData e

-- | Convert 'CellData' to a cell
dataCell :: CellData -> X.Cell
dataCell cd =
  X.def
    & case cd of
      CellValue d -> X.cellValue ?~ d
      CellFormula d -> X.cellFormula ?~ d
      CellComment d -> X.cellComment ?~ d
      CellEmpty -> X.def
