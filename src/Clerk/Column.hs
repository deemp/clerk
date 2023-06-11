module Clerk.Column where

import Clerk.Coordinates (col)
import Clerk.Format (blank)
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
column :: forall a input output. Maybe Double -> FormatCell -> (input -> output) -> RowIO input output (Ref a)
column _width _fmtCell _mkOutput = do
  state_ <- get
  let _columnsProperties =
        Just $
          (unColumnsProperties def)
            { X.cpMin = state_ ^. col . to fromIntegral
            , X.cpMax = state_ ^. col . to fromIntegral
            , X.cpWidth = _width
            }
  tell (Template [CellTemplate{_fmtCell, _mkOutput, _columnsProperties}])
  cell <- gets Ref
  modify (col +~ 1)
  pure cell

column_ :: forall input output. Maybe Double -> FormatCell -> (input -> output) -> RowIO input output ()
column_ _width _fmtCell _mkOutput = void $ column _width _fmtCell _mkOutput

-- | A column with a given width and cell format. Returns a cell reference
columnWF :: forall a input output. ToCellData output => Double -> FormatCell -> (input -> output) -> RowI input (Ref a)
columnWF width fmtCell mkOutput = do
  state_ <- get
  column (Just width) fmtCell (fst . runWriter . flip evalStateT state_ . _rowIO . toCellData . mkOutput)

-- | A column with a given width and cell format
columnWF_ :: forall input output. ToCellData output => Double -> FormatCell -> (input -> output) -> RowI input ()
columnWF_ width fmtCell mkOutput = void (columnWF width fmtCell mkOutput)

-- | A column with a given cell format. Returns a cell reference
columnF :: forall a input output. ToCellData output => FormatCell -> (input -> output) -> RowI input (Ref a)
columnF fmtCell mkOutput = do
  state_ <- get
  column Nothing fmtCell (fst . runWriter . flip evalStateT state_ . _rowIO . toCellData . mkOutput)

columnF_ :: forall input output. ToCellData output => FormatCell -> (input -> output) -> RowI input ()
columnF_ fmtCell mkOutput = void (columnF fmtCell mkOutput)

columnIO :: forall a input output. ToCellData output => (input -> output) -> RowI input (Ref a)
columnIO = columnF blank

columnIO_ :: forall input output. ToCellData output => (input -> output) -> RowI input ()
columnIO_ mkOutput = void (columnF blank mkOutput)

columnO :: forall a input output. ToCellData output => output -> RowI input (Ref a)
columnO output = columnF blank (const output)

columnO_ :: forall input output. ToCellData output => output -> RowI input ()
columnO_ output = void (columnF blank (const output))