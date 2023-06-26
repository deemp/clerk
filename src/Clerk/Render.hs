module Clerk.Render where

import Clerk.Coordinates
import Clerk.Row
import Clerk.Sheet
import Clerk.Transform
import qualified Codec.Xlsx as X
import Control.Monad
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), evalStateT)
import Control.Monad.Trans.Writer
import Data.Default
import Data.Foldable
import Data.Function
import Data.List (zipWith4)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Lens.Micro
import Data.Bifunctor (Bifunctor(..))

type RenderTemplate input output = ToCellData output => RowState -> InputIndex -> input -> Template input output -> Sheet Transform
type RenderInputs input output a = ToCellData output => [input] -> (input -> RowIO input output a) -> Sheet (Transform, a)

-- | Render inputs starting at given coords using a function producing a 'RowIO'. Return the results calculated for each row.
renderInputsFRs :: RowState -> RenderTemplate input output -> ToCellData output => [input] -> (input -> RowIO input output a) -> Sheet (Transform, [a])
renderInputsFRs state render inputs f = do
  let
    ts =
      [ (newState, template, res)
      | (inputIndex, input) <- zip [0 :: Int ..] inputs
      , let
          newState = (state & row +~ fromIntegral inputIndex){_inputIndex = fromIntegral inputIndex}
          (res, template) = runRow (f input) newState
      ]
    -- result obtained from the top row
    rowResults = ts ^.. traversed . _3
    transform = fold <$> sequenceA (zipWith4 render (ts ^.. traversed . _1) [0 ..] inputs (ts ^.. traversed . _2))
  (,rowResults) <$> transform

-- | Render inputs starting at given coords using a function producing a 'RowIO'. Return the result calculated using the topmost row.
renderInputsF :: RowState -> RenderTemplate input output -> RenderInputs input output a
renderInputsF state render inputs f = renderInputsFRs state render inputs f <&> second head

-- | Render inputs starting at given coords and using a 'RowIO'. Return the result calculated using the topmost row.
renderInputs :: RowState -> RenderTemplate input output -> ToCellData output => [input] -> RowIO input output a -> Sheet (Transform, a)
renderInputs state render inputs f = renderInputsF state render inputs (const f)

-- | Render a template with a given offset, input index and input
renderTemplate :: RenderTemplate input output
renderTemplate leftCell inputIndex input (Template cells) = do
  ps <-
    zipWithM
      ( \columnIndex cellTemplate -> do
          let
            CellTemplate{..} = cellTemplate
            cellCol = leftCell ^. col + columnIndex
          -- TODO it typechecks now, but is this correct?
          cellCoords <- mkCoords' cellCol (leftCell ^. row)
          let cellData_ = toCellData (_mkOutput input) & _rowIO & flip evalStateT cellCoords & runWriter & fst
              cell = _fmtCell inputIndex cellCoords cellData_ & _rowIO & flip evalStateT cellCoords & runWriter & fst
              _wsTransform
                -- add column width only once
                -- new properties precede old properties
                | inputIndex == 0 = X.wsColumnsProperties %~ (maybeToList _columnsProperties ++)
                | otherwise = id
          newCoords <- mkCoords' cellCol (leftCell ^. row)
          let _fmTransform = Map.insert (fromCoords newCoords) cell
          pure def{_fmTransform, _wsTransform}
      )
      [0 ..]
      cells
  pure $ fold ps

-- | Render inputs starting at given coords and using a row. Return the results calculated for each row
renderInputsResults ::
  (ToCellData output, Integral i) =>
  forall a input.
  [i] ->
  RowState ->
  RenderTemplate input output ->
  ([input] -> [a -> RowIO input output a] -> a -> Sheet (Transform, [a]))
renderInputsResults is state render inputs fs a = do
  let
    newStates = (\inputIndex -> (state & row +~ fromIntegral inputIndex){_inputIndex = fromIntegral inputIndex}) <$> is
    ts = snd $
      runWriter $
        flip evalStateT a $
          forM (zip fs newStates) $ \(f, state_) -> do
            a1 <- get
            let (a2, t) = runRow (f a1) state_
            lift $ tell [(a2, t)]
            put a2
    transform = fold <$> sequenceA (zipWith4 render newStates [0 ..] inputs (snd <$> ts))
  (,fst <$> ts) <$> transform