{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Convert where

import Control.Monad (forM_)
import Converter
import Data.String.Interpolate (i)
import qualified Data.Text.IO as T

config :: Config User
config =
  def
    & indent ?~ "i"
    & dedent ?~ "d"
    & enable ?~ "E"
    & disable ?~ "D"
    & texHaskellCodeStart ?~ "\\begin{mycode}"
    & texHaskellCodeEnd ?~ "\\end{mycode}"

chapterSuffixes :: [String]
chapterSuffixes = ["4_1", "4_2", "4_3", "4_4"]

chapterTex :: String -> FilePath
chapterTex x = [i|../thesis/chapters/chapter4-#{x}.tex|]

chapterHs :: String -> FilePath
chapterHs x = [i|src/Chapters/Chapter4/Example#{x}/Main.hs|]

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
