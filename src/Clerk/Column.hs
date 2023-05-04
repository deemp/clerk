module Clerk.Column where

import Clerk.Coordinates (col)
import Clerk.Reference (Ref (Ref))
import Clerk.Row (CellTemplate (..), FormatCell, RowI, RowIO (..), Template (..), ToCellData (..))
import qualified Codec.Xlsx as X
import Control.Monad.RWS.Class (MonadState (get), MonadWriter (tell), gets, modify)
import Control.Monad.State (evalStateT, void)
import Control.Monad.Writer (runWriter)
import Data.Default (Default (..))
import Lens.Micro (to, (+~), (^.))

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
  state_ <- get
  let columnsProperties =
        Just $
          (unColumnsProperties def)
            { X.cpMin = state_ ^. col . to fromIntegral
            , X.cpMax = state_ ^. col . to fromIntegral
            , X.cpWidth = width
            }
  tell (Template [CellTemplate{fmtCell, mkOutput, columnsProperties}])
  cell <- gets Ref
  modify (col +~ 1)
  pure cell

-- | A column with a given width and cell format. Returns a cell reference
columnWidthRef :: ToCellData output => Double -> FormatCell -> (input -> output) -> RowI input (Ref a)
columnWidthRef width fmtCell mkOutput = do
  state_ <- get
  columnWidthFormatRef (Just width) fmtCell (fst . runWriter . flip evalStateT state_ . _rowIO . toCellData . mkOutput)

-- | A column with a given width and cell format
columnWidth :: ToCellData output => Double -> FormatCell -> (input -> output) -> RowI input ()
columnWidth width fmtCell mkOutput = void (columnWidthRef width fmtCell mkOutput)

-- | A column with a given cell format. Returns a cell reference
columnRef :: ToCellData output => FormatCell -> (input -> output) -> RowI input (Ref a)
columnRef fmtCell mkOutput = do
  state_ <- get
  columnWidthFormatRef Nothing fmtCell (fst . runWriter . flip evalStateT state_ . _rowIO . toCellData . mkOutput)

-- | A column with a given cell format
column :: ToCellData output => FormatCell -> (input -> output) -> RowI input ()
column fmtCell mkOutput = void (columnRef fmtCell mkOutput)