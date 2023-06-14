-- | Naming principle
-- I used these abbreviations and then removed the redundant ones from name suffices
-- Ixs - Indices
-- Ix - Index
-- Ins - Inputs
-- In - Input
-- Fs - Functions
-- F - Function
-- Cs - Constant functions
-- C - Constant function
module Clerk.Place where

import Clerk.Coordinates
import Clerk.Render
import Clerk.Row
import Clerk.Sheet
import Control.Monad
import Control.Monad.RWS (MonadWriter (tell))

-- | Starting at a given coordinate, place a list of inputs according to a row builder and return a result
placeInsFRs :: (ToCellData output, ToCoords c) => c -> [input] -> (input -> RowIO input output a) -> Sheet [a]
placeInsFRs (toCoords -> state) inputs mkRow = do
  transformResult <- renderInputsFRs state renderTemplate inputs mkRow
  tell (fst transformResult)
  pure (snd transformResult)

-- | Starting at a given coordinate, place a list of inputs according to a row builder and return a result
placeInsF :: (ToCellData output, ToCoords c) => c -> [input] -> (input -> RowIO input output a) -> Sheet a
placeInsF (toCoords -> state) inputs mkRow = do
  transformResult <- renderInputsF state renderTemplate inputs mkRow
  tell (fst transformResult)
  pure (snd transformResult)

-- | Starting at a given coordinate, place a list of inputs according to a row builder and return a result
placeIns :: (ToCellData output, ToCoords c) => c -> [input] -> RowIO input output a -> Sheet a
placeIns state inputs row_ = placeInsF state inputs (const row_)

placeIns_ :: (ToCellData output, ToCoords c) => c -> [input] -> RowIO input output a -> Sheet ()
placeIns_ state inputs row_ = void $ placeIns state inputs row_

-- | Starting at a given coordinate, place one input according to a row builder and return a result
placeIn :: (ToCellData output, ToCoords c) => c -> input -> RowIO input output a -> Sheet a
placeIn coords_ input = placeIns coords_ [input]

placeIn_ :: (ToCellData output, ToCoords c) => c -> input -> RowIO input output a -> Sheet ()
placeIn_ coords_ input row_ = void $ placeIns coords_ [input] row_

-- | Starting at a given coordinate, place a row builder and return a result
place :: (ToCellData output, ToCoords c) => c -> RowO output a -> Sheet a
place coords_ = placeIn coords_ ()

-- | Starting at a given coordinate, place a row builder and return a result
place_ :: (ToCellData output, ToCoords c) => c -> RowO output a -> Sheet ()
place_ coords_ = void <$> placeIn coords_ ()

-- --- Composable row functions

-- | Starting at a given coordinate, place a list of inputs according to row builders and return a result
placeIxsInsFs :: (ToCellData output, ToCoords c, Integral index) => c -> [index] -> [input] -> [a -> RowIO input output a] -> a -> Sheet [a]
placeIxsInsFs (toCoords -> state) is inputs fs a = do
  transformResult <- renderInputsResults is state renderTemplate inputs fs a
  tell (fst transformResult)
  pure (snd transformResult)

placeIxsInsFs_ :: (ToCellData output, ToCoords c, Integral index) => c -> [index] -> [input] -> [a -> RowIO input output a] -> a -> Sheet ()
placeIxsInsFs_ state is inputs fs a = void $ placeIxsInsFs state is inputs fs a

-- | Takes indices from input indices
placeInsFs :: (ToCellData output, ToCoords c) => c -> [input] -> [a -> RowIO input output a] -> a -> Sheet [a]
placeInsFs state inputs = placeIxsInsFs state [0 .. length inputs - 1] inputs

placeInsFs_ :: (ToCellData output, ToCoords c) => c -> [input] -> [a -> RowIO input output a] -> a -> Sheet ()
placeInsFs_ state inputs fs a = void $ placeIxsInsFs state [0 .. length inputs - 1] inputs fs a

-- | 'a' may contain an index
placeIxsFs :: (ToCellData output, ToCoords c, Integral index) => c -> [index] -> [a -> RowO output a] -> a -> Sheet [a]
placeIxsFs state is = placeIxsInsFs state is (repeat ())

placeIxsFs_ :: (ToCellData output, ToCoords c, Integral index) => c -> [index] -> [a -> RowO output a] -> a -> Sheet ()
placeIxsFs_ state is fs a = void $ placeIxsInsFs state is (repeat ()) fs a

placeIxsCs :: (ToCellData output, ToCoords c, Integral index) => c -> [index] -> [RowO output a] -> a -> Sheet [a]
placeIxsCs state is fs = placeIxsInsFs state is (repeat ()) (const <$> fs)

placeIxsCs_ :: (ToCellData output, ToCoords c, Integral index) => c -> [index] -> [RowO output a] -> a -> Sheet ()
placeIxsCs_ state is fs a = void $ placeIxsInsFs state is (repeat ()) (const <$> fs) a

placeIxF :: (ToCellData output, ToCoords c, Integral index) => c -> index -> (a -> RowO output a) -> a -> Sheet [a]
placeIxF state is fs = placeIxsInsFs state [is] (repeat ()) [fs]

placeIxF_ :: (ToCellData output, ToCoords c, Integral index) => c -> index -> (a -> RowO output a) -> a -> Sheet ()
placeIxF_ state is fs a = void $ placeIxsInsFs state [is] (repeat ()) [fs] a