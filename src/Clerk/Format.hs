{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Clerk.Format where

import Clerk.Row (FormatCell, ToCellData (toCellData), dataCell)
import qualified Codec.Xlsx as X
import qualified Codec.Xlsx.Formatted as X
import Data.Text (Text)
import Lens.Micro ((&), (.~), (?~))

-- TODO use Color

-- | something that can be turned into ARGB
class ToARGB a where
  toARGB :: a -> Text

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
                                    ?~ (X.def & X.colorARGB ?~ toARGB color)
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
  pure $ X.def & X.formattedCell .~ dataCell cd
