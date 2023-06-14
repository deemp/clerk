{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Convert (convert) where

import Control.Monad (forM_)
import Converter
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import Data.Text.IO qualified as T

config :: Config User
config =
  def
    & indent ?~ "i"
    & dedent ?~ "d"
    & enable ?~ "E"
    & disable ?~ "D"
    & texHaskellCodeStart ?~ "\\begin{minted}"
    & texHaskellCodeEnd ?~ "\\end{minted}"

srcChapter :: Int -> FilePath
srcChapter x = [i|Chapters/Chapter#{x}|]
texChapter :: Int -> FilePath
texChapter x = [i|../thesis/chapters/Chapter#{x}|]

data ChapterMapping = ChapterMapping {hsDir :: FilePath, texDir :: FilePath, moduleName :: FilePath}
chapterMappings :: [ChapterMapping]
chapterMappings =
  [ 1 :: Int
  , 2
  , 3
  -- , 4
  ]
    <&> (\x -> ChapterMapping{hsDir = srcChapter 4, texDir = texChapter 4, moduleName = [i|Example#{x}|]})

chapterTex :: ChapterMapping -> FilePath
chapterTex ChapterMapping{..} = [i|#{texDir}/#{moduleName}.tex|]

chapterHs :: ChapterMapping -> FilePath
chapterHs ChapterMapping{..} = [i|#{hsDir}/#{moduleName}.hs|]

selectChapter :: Format -> (ChapterMapping -> FilePath)
selectChapter = \case
  TeX -> chapterTex
  Hs -> chapterHs
  _ -> error "selectChapter undefined for other formats"

convert :: Format -> Format -> IO ()
convert formatFrom formatTo =
  forM_ chapterMappings $ \x -> do
    T.readFile (selectChapter formatFrom x)
      >>= T.writeFile (selectChapter formatTo x)
        . (formatFrom `convertTo` formatTo) config
