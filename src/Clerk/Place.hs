module Clerk.Place where

import Clerk.Coordinates
import Clerk.Render (renderInputs, renderTemplate)
import Clerk.Row
import Clerk.Sheet
import Control.Monad.RWS (MonadWriter (tell))

-- | Starting at a given coordinate, place a list of inputs according to a row builder and return a result
placeN :: (ToCellData output, ToCoords c) => c -> [input] -> RowIO input output a -> Sheet a
placeN (toCoords -> state) inputs b = do
  transformResult <- renderInputs state renderTemplate inputs b
  tell (fst transformResult)
  pure (snd transformResult)

-- | Starting at a given coordinate, place one input according to a row builder and return a result
place1 :: (ToCellData output, ToCoords c) => c -> input -> RowIO input output a -> Sheet a
place1 coords_ input = placeN coords_ [input]

-- | Starting at a given coordinate, place a row builder and return a result
place :: (ToCellData output, ToCoords c) => c -> RowO output a -> Sheet a
place coords_ = place1 coords_ ()
