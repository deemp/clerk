{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module ForExamples where

import Clerk
import Data.Default (Default (..))
import Data.Text (Text)

mkRef :: forall a. Int -> Int -> Ref a
mkRef _col _row = fromCoords def{_col, _row}

showFormula :: RowShow a => a -> Text
showFormula a = evalRow (rowShow a) def