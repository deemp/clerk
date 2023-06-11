{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Clerk.Reference where

import Clerk.AddressTyped
import Clerk.Coordinates
import Clerk.Internal
import Codec.Xlsx (ColumnIndex, RowIndex)
import Data.Default

-- | A typed reference to a cell.
--
-- The user is responsible for setting the necessary cell type.
--
-- The type prevents operations between cell references with incompatible types.
--
-- @
-- >>>str = undefined :: Ref T.Text
-- >>>str .+ str
-- No instance for (Num Text) arising from a use of `.+'
-- In the expression: str .+ str
-- In an equation for `it_aezf8': it_aezf8 = str .+ str
--
-- @
-- When necessary, one can UNSAFELY change the cell reference type via 'as'
--
-- @
-- >>>int = undefined :: Ref Int
-- >>>double = undefined :: Ref Double
-- >>>as int .+ double
-- Couldn't match expected type `Coords'
--             with actual type `Text -> FilePath -> Coords'
-- Probable cause: `Coords' is applied to too few arguments
-- In the first argument of `Ref', namely `(Coords 1 1)'
-- In the expression: Ref (Coords 1 1) :: Ref Int
-- In an equation for `int': int = Ref (Coords 1 1) :: Ref Int
--
-- @
newtype Ref a = Ref {unRef :: Coords}
  deriving (Num) via Coords

instance ToCoords (Ref a) where
  toCoords :: Ref a -> Coords
  toCoords = unRef

instance FromCoords (Ref a) where
  fromCoords :: Coords -> Ref a
  fromCoords = Ref

instance UnsafeChangeType Ref where
  unsafeChangeType :: Ref b -> Ref c
  unsafeChangeType (Ref c) = Ref c