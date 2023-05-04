module Clerk.Sheet where

import Clerk.Coordinates (Coords (..))
import Clerk.Transform (Transform)
import Codec.Xlsx (ColumnIndex, RowIndex)
import Control.Monad.State (MonadState (get), StateT, evalStateT)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Control.Monad.Writer (MonadWriter)

data SheetState = SheetState
  { _sheetWorksheetName :: String
  , _sheetWorkbookPath :: FilePath
  }

-- | A builder to compose the results of 'Transform's
newtype Sheet a = Sheet {_sheet :: StateT SheetState (Writer Transform) a}
  deriving newtype (Functor, Applicative, Monad, MonadWriter Transform, MonadState SheetState)

-- | Evaluate the result of a sheet with a default state
evalSheetDefault :: Sheet a -> a
evalSheetDefault s = fst $ runWriter $ flip evalStateT (SheetState{_sheetWorksheetName = "worksheet", _sheetWorkbookPath = "workbook"}) $ _sheet s

-- | Make 'Coords' from a column index and a row index
mkCoords :: ColumnIndex -> RowIndex -> Sheet Coords
mkCoords _col _row = do
  SheetState{_sheetWorkbookPath = _coordsWorkbookPath, _sheetWorksheetName = _coordsWorksheetName} <- get
  pure Coords{_col, _row, ..}