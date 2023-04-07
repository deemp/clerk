module Main (main) where

import Control.Monad (forM, (>=>))
import Converter (Format (..), convertTo, def)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T

main :: IO ()
main = do
  intro <- T.readFile "README/Intro.md"
  let examplesPaths = (\(x :: Int) -> [i|app/Example#{x}.hs|]) <$> [1 .. 4]
  examples <- forM examplesPaths (T.readFile >=> (pure . (Hs `convertTo` Md) def))
  outro <- T.readFile "README/Outro.md"
  T.writeFile "README.md" (T.intercalate "\n" ([intro] <> examples <> [outro]))