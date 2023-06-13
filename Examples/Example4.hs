{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Clerk
import Control.Monad (void)
import Control.Monad.RWS (gets)

main :: IO ()
main = writeXlsx "example4.xlsx" [("List 1", sheet 9 15)]

colFormula :: ToCellData output => output -> RowI input (Ref a)
colFormula = columnF blank . const

colIndex :: InputIndex -> RowIO input CellData ()
colIndex = void . colFormula

index :: RowO CellData InputIndex
index = gets ((+ 1) . _inputIndex)

row0 :: Int -> Int -> Row (Ref Int, Ref Int)
row0 a b = do
  colIndex =<< index
  r1 <- colFormula a
  r2 <- colFormula b
  pure (r1, r2)

row1 :: (Ref Int, Ref Int) -> Row (Ref Int, Ref Int)
row1 (a, b) =
  do
    colIndex =<< index
    r1 <- colFormula (fun "MAX" [a, b] :: Formula Int)
    r2 <- colFormula (fun "MIN" [a, b] :: Formula Int)
    pure (r1, r2)

row3 :: (Ref Int, Ref Int) -> Row (Ref Int, Ref Int)
row3 (a, b) = do
  colIndex =<< index
  r1 <- colFormula (formulaRef a)
  r2 <- colFormula (formulaRef b)
  r3 <- colFormula (fun "MOD" [r1, r2] :: Formula Int)
  pure (r2, r3)

sheet :: Int -> Int -> Sheet ()
sheet a b = do
  start <- mkRef' @"A1"
  s1 <- place start (row0 a b)
  placeIxsFs_ start [1 :: Int .. 6] (cycle [row1, row3]) s1
  pure ()