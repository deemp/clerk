{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Clerk.PlaceMonadic where

-- TODO
-- somehow pipe rows
(>>=) :: forall a b m. a %m -> (a %m -> b) %1 -> b
v >>= f = f v
