{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.Helpers where

import Clerk
import Data.Default
import Data.Text

-- | Make a reference with default coordinates
mkRef :: forall address column row c a. (Address' address column row c) => Ref a
mkRef = fromCoords def{_col, _row}
 where
  (_col, _row) = mkAddress @address

showFormula :: (RowShow a) => a -> Text
showFormula = rowShowDefault