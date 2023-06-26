{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Clerk.Reference where

import Clerk.Coordinates (Coords, FromCoords (..), ToCoords (..))
import Clerk.Internal (UnsafeChangeType (..))

-- | A typed reference to a cell.
--
-- A user is responsible for setting the necessary cell type.
--
-- This type prevents operations between cell references with incompatible types.
-- @
newtype Ref a = Ref {unRef :: Coords}
  deriving (Num, ToCoords, FromCoords) via Coords

instance UnsafeChangeType Ref where
  unsafeChangeType :: Ref b -> Ref c
  unsafeChangeType (Ref c) = Ref c
