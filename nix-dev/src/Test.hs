{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test where

import Control.Monad (forM_)
import Converter (Config, Format (Hs, TeX), User, convertTo, def, texHaskellCodeEnd, texHaskellCodeStart, (&), (?~))
import Data.String.Interpolate (i)
import Data.Text.IO qualified as T

config :: Config User
config =
  def
    & texHaskellCodeStart ?~ "\\begin{mycode}"
    & texHaskellCodeEnd ?~ "\\end{mycode}"

chapterSuffixes :: [String]
chapterSuffixes = ["Clerk", "4_1", "4_2", "4_3"]

chapterTex :: String -> FilePath
chapterTex "Clerk" = [i|../thesis/chapters/chapter3.tex|]
chapterTex x = [i|../thesis/chapters/chapter#{x}.tex|]

chapterHs :: String -> FilePath
chapterHs "Clerk" = [i|src/Clerk.hs|]
chapterHs x = [i|src/Chapter#{x}.hs|]

selectChapter :: Format -> (String -> FilePath)
selectChapter = \case
  TeX -> chapterTex
  Hs -> chapterHs

convert :: Format -> Format -> IO ()
convert formatFrom formatTo =
  forM_ chapterSuffixes $ \x -> do
    T.readFile (selectChapter formatFrom x)
      >>= T.writeFile (selectChapter formatTo x)
        . (formatFrom `convertTo` formatTo) config
