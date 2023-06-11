{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Clerk.PlaceMonadicTry where

import Clerk.Coordinates (row)
import Clerk.PlaceMonadic as PM
import Clerk.Row
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Default (Default (..))
import Lens.Micro

p1 :: RowIO i o a
p1 = undefined
p2 :: a -> RowIO i o b
p2 = undefined
p3 :: b -> a -> RowIO i o a
p3 = undefined

-- this monad should have a list of indices and the initial coordinate as the state
-- we can't put rows into a list as they can be incompatible

-- t = 3
--  where
--   idxs = [1, 2, 3]
--   initState = def @RowState
--   place 
--   p1' = runWriter (runStateT p1._rowIO (initState & row .~ idxs !! 0)) ^. _1 . _1
--   p2' = runWriter (runStateT (p2 p1')._rowIO (initState & row .~ idxs !! 1)) ^. _1 . _1
--   p3' = runWriter (runStateT (p3 p2' p1')._rowIO (initState & row .~ idxs !! 2)) ^. _1 . _1
  

-- t = do

--   p1' <- p1
--   pure _

-- p :: [RowIO input output a]
-- p = PM.do
--   pure []
