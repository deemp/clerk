module Clerk.Internal where

import Data.Kind (Type)

-- | Change the type of something. Use with caution!
class UnsafeChangeType (a :: Type -> Type) where
  unsafeChangeType :: forall c b. a b -> a c

-- | @UNSAFELY@ change the type of something wrapped
as :: forall c b a. UnsafeChangeType a => a b -> a c
as = unsafeChangeType
