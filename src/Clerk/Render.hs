module Clerk.Render where

import Clerk.Coordinates
import Clerk.Row
import Clerk.Sheet
import Clerk.Transform
import qualified Codec.Xlsx as X
import Control.Monad
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Writer
import Data.Default
import Data.Foldable
import Data.Function
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Lens.Micro

type RenderTemplate m input output = (Monad m, ToCellData output) => RowState -> InputIndex -> input -> Template input output -> Sheet Transform
type RenderInputs m input output a = (Monad m, ToCellData output) => [input] -> RowIO input output a -> Sheet (Transform, a)

-- | Render inputs starting at given coords and using a row. Return the result calculated using the topmost row
renderInputs :: RowState -> RenderTemplate Sheet input output -> RenderInputs Sheet input output a
renderInputs state render inputs row_ = do
  let
    ts =
      [ (newState, template)
      | inputRow <- [0 .. length inputs - 1]
      , let newState = state & row +~ fromIntegral inputRow
            template = execRow row_ newState
      ]
    -- result obtained from the top row
    rowResult = evalRow row_ state
    transform =
      fold
        <$> sequenceA
          ( zipWith3
              ( \input inputIndex (st, template) ->
                  render st inputIndex input template
              )
              inputs
              [0 ..]
              ts
          )
  (,rowResult) <$> transform

-- | Render a template with a given offset, input index and input
renderTemplate :: RenderTemplate Sheet input output
renderTemplate state inputIndex input (Template columns) = do
  ps <-
    zipWithM
      ( \columnIndex cellTemplate -> do
          let
            CellTemplate{..} = cellTemplate
            leftCell = state
            cellCol = leftCell ^. col + columnIndex
          -- TODO it typechecks now, but is this correct?
          cellCoords <- mkCoords cellCol (leftCell ^. row)
          let cellData_ = toCellData (mkOutput input) & _rowIO & flip evalStateT cellCoords & runWriter & fst
              cell = fmtCell cellCoords inputIndex cellData_ & _rowIO & flip evalStateT cellCoords & runWriter & fst
          let wsTransform
                -- add column width only once
                -- new properties precede old properties
                | inputIndex == 0 = X.wsColumnsProperties %~ (maybeToList columnsProperties ++)
                | otherwise = id
          newCoords <- mkCoords cellCol (leftCell ^. row)
          let fmTransform = Map.insert (fromCoords newCoords) cell
          pure def{fmTransform, wsTransform}
      )
      [0 ..]
      columns
  pure $ fold ps
