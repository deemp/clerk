{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | @Clerk@ library
module Clerk (
  module Clerk.AddressTyped,
  module Clerk.ColorTyped,
  module Clerk.Column,
  module Clerk.Coordinates,
  module Clerk.Expression,
  module Clerk.Format,
  module Clerk.Formula,
  module Clerk.Function,
  module Clerk.Internal,
  module Clerk.Operators,
  module Clerk.Place,
  module Clerk.Reference,
  module Clerk.Render,
  module Clerk.Row,
  module Clerk.Sheet,
  module Clerk.Symbols,
  module Clerk.Transform,
  module Clerk.Xlsx,
) where

import Clerk.AddressTyped
import Clerk.ColorTyped
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
import Clerk.Render
import Clerk.Row
import Clerk.Sheet
import Clerk.Symbols
import Clerk.Transform
import Clerk.Xlsx

-- TODO add modes to state
-- Google Sheets, Excel, Tabular

-- TODO Formula example

-- TODO multiple sheets example

-- TODO newtype
