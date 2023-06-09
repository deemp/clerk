{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Clerk
import Clerk.Row
import Clerk.Sheet (mkRef')
import Control.Lens ((&), (+~))
import Control.Monad (void)
import Control.Monad.RWS (gets)

main :: IO ()
main = writeXlsx "example4.xlsx" [("List 1", sheet 9 15)]

colFun :: ToCellData output => output -> RowI input (Ref a)
colFun = columnF blank . const

colRef_ :: InputIndex -> RowIO input CellData ()
colRef_ = void . colFun

index :: RowO CellData InputIndex
index = gets ((+ 1) . _inputIndex)

row0 :: Int -> Int -> Row (Ref Int, Ref Int)
row0 a b = do
  colRef_ =<< index
  r1 <- colFun a
  r2 <- colFun b
  pure (r1, r2)

row1 :: (Ref Int, Ref Int) -> Row (Ref Int, Ref Int)
row1 (a, b) =
  do
    colRef_ =<< index
    r1 <- colFun (fun "MAX" [a, b] :: Formula Int)
    r2 <- colFun (fun "MIN" [a, b] :: Formula Int)
    pure (r1, r2)

row3 :: (Ref Int, Ref Int) -> Row (Ref Int, Ref Int)
row3 (a, b) = do
  colRef_ =<< index
  r1 <- colFun (funRef a)
  r2 <- colFun (funRef b)
  r3 <- colFun (fun "MOD" [r1, r2] :: Formula Int)
  pure (r2, r3)

sheet :: Int -> Int -> Sheet ()
sheet a b = do
  start <- mkRef' @"A1"
  s1 <- place start (row0 a b)
  placeIxsFs_ start [1 .. 6] (cycle [row1, row3]) s1
  pure ()