{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | @Clerk@ library
module Clerk (
  -- * Coords
  Coords (..),
  mkCoords,
  ToCoords (..),
  FromCoords (..),
  CoordsLike,

  -- * Cell references
  Ref,
  row,
  col,
  funRef,
  val,

  -- * Changing types
  UnsafeChangeType (..),
  as,

  -- * Cell formatting
  InputIndex,
  FormatCell,
  CellTemplate,
  FormattedMap,
  FMTransform,
  WSTransform,
  Transform,
  FCTransform,
  horizontalAlignment,
  mkColor,
  blank,
  ToARGB (..),

  -- * Templates

  -- * Columns
  module Clerk.Column,

  -- * Sheet builder
  Sheet,
  SheetState (..),
  module Clerk.Place,
  evalSheetDefault,

  -- * Row builder
  RowShow (..),

  -- * Expressions
  Expr,
  Formula,
  ToFormula (..),
  NumOperator,
  (.+),
  (.-),
  (.*),
  (./),
  (.:),
  (.^),
  (.^^),
  (.**),
  (.<),
  (.>),
  (.<=),
  (.>=),
  (.=),
  (.<>),
  (.&),
  fun,
  -- TODO work on default types
  Range,
  FunctionName,

  -- * Cells
  ToCellData (..),

  -- * xlsx
  composeXlsx,
  writeXlsx,

  -- * For examples
  mkRefDefault,
) where

import Clerk.Column
import Clerk.Coordinates
import Clerk.Expression
import Clerk.Format
import Clerk.Formula
import Clerk.Function
import Clerk.Internal
import Clerk.Operators
import Clerk.Place
import Clerk.Reference
import Clerk.Row
import Clerk.Sheet
import Clerk.Transform
import Clerk.Xlsx
import Clerk.ForExamples

-- TODO add modes to state
-- Google Sheets, Excel, Tabular

-- TODO Formula example

-- TODO multiple sheets example

-- TODO newtype
