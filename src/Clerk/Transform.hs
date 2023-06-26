module Clerk.Transform where

import Clerk.Row (FormatCell)
import qualified Codec.Xlsx as X
import qualified Codec.Xlsx.Formatted as X
import Data.Default (Default (..))
import Data.Function ((&))
import qualified Data.Map as Map
import Lens.Micro (non, (%~))

-- | Map of coordinates to cell formatting
type FormattedMap = Map.Map (X.RowIndex, X.ColumnIndex) X.FormattedCell

-- | Transform of a map that maps coordinates to cell formatting
type FMTransform = FormattedMap -> FormattedMap

-- | Transform of a worksheet
type WSTransform = X.Worksheet -> X.Worksheet

-- | Combined: a transform of a map of formats and a transform of a worksheet
data Transform = Transform {_fmTransform :: FMTransform, _wsTransform :: WSTransform}

instance Semigroup Transform where
  (<>) :: Transform -> Transform -> Transform
  (Transform a1 b1) <> (Transform a2 b2) = Transform (a2 . a1) (b2 . b1)

instance Monoid Transform where
  mempty :: Transform
  mempty = Transform id id

instance Default Transform where
  def :: Transform
  def = mempty

-- | Transform of a formatted cell
type FCTransform = X.FormattedCell -> X.FormattedCell

-- | Apply 'FCTransform' to a 'FormatCell' to get a new 'FormatCell'
(.&) :: FormatCell -> FCTransform -> FormatCell
fc .& ft = \coords_ index cd -> ft <$> fc coords_ index cd

infixl 5 .&

-- | Get a 'FCTransform' with a given horizontal alignment in a cell
horizontalAlignment :: X.CellHorizontalAlignment -> FCTransform
horizontalAlignment alignment fc =
  fc & X.formattedFormat . X.formatAlignment . non X.def . X.alignmentHorizontal %~ const (Just alignment)

-- | Get a 'FCTransform' with a given vertical alignment in a cell
verticalAlignment :: X.CellVerticalAlignment -> FCTransform
verticalAlignment alignment fc =
  fc & X.formattedFormat . X.formatAlignment . non X.def . X.alignmentVertical %~ const (Just alignment)
