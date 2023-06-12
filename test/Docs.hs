module Main (main) where

import Control.Monad (forM, (>=>))
import Converter (Format (Hs, Md), convertTo, def)
import Data.String.Interpolate (i)
import Data.Text as T (intercalate)
import Data.Text.IO as T (readFile, writeFile)

main :: IO ()
main = do
  intro <- T.readFile "README/Intro.md"
  let examplesPaths = (\(x :: Int) -> [i|Examples/Example#{x}.hs|]) <$> [1 .. 4]
  examples <- forM examplesPaths (T.readFile >=> (pure . (Hs `convertTo` Md) def))
  outro <- T.readFile "README/Outro.md"
  T.writeFile "README.md" (T.intercalate "\n" ([intro] <> examples <> [outro]))