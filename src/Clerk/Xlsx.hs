module Clerk.Xlsx where

import Clerk.Sheet (Sheet (_sheet), SheetState (SheetState, _sheetWorkbookPath, _sheetWorksheetName))
import Clerk.Transform (Transform (..))
import qualified Codec.Xlsx as X
import qualified Codec.Xlsx.Formatted as X
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Writer (execWriter)
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lens.Micro ((%~))

-- | Compose an @xlsx@ from a list of sheet names and builders
composeXlsx :: FilePath -> [(T.Text, Sheet ())] -> X.Xlsx
composeXlsx path sheetBuilders = workBookWithColumnWidths
 where
  getTransform _sheetWorksheetName x =
    execWriter
      $ flip
        evalStateT
        (SheetState{_sheetWorkbookPath = path, ..})
      $ _sheet x
  workBookWithData =
    flip X.formatWorkbook X.def $
      (\(name, tf) -> (name, (getTransform (T.unpack name) tf & _fmTransform) X.def))
        <$> sheetBuilders
  workBookWithColumnWidths =
    workBookWithData
      & X.xlSheets
        %~ \sheets ->
          zipWith
            ( \x (name, ws) ->
                ( name
                , ws
                    & (getTransform (T.unpack name) x & _wsTransform)
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
