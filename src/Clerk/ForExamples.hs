{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Clerk.ForExamples where

import Clerk.AddressTyped
import Clerk.Coordinates
import Clerk.Internal
import Clerk.Reference
import Codec.Xlsx (ColumnIndex, RowIndex, Formula)
import Data.Default
import Data.Text
import Clerk.Row (RowShow, rowShowDefault)

-- | Make a reference with default coordinates
mkRefDefault :: forall address column row c a. Address' address column row c => Ref a
mkRefDefault = fromCoords def{_col, _row}
 where
  (_col, _row) = mkAddress @address

showFormula :: RowShow a => a -> Text
showFormula = rowShowDefault