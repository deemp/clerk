{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Clerk
import Codec.Xlsx (Border, BorderStyle)
import qualified Codec.Xlsx as X
import Codec.Xlsx.Formatted (FormattedCell (_formattedColSpan, _formattedRowSpan))
import qualified Codec.Xlsx.Formatted as X
import Control.Lens (Lens', non, (%~), (&), (+~), (-~), (<&>), (^.))
import Control.Monad.Identity
import Data.Data (Data (toConstr))
import Data.List (zipWith4, zipWith5)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

dataTotalStarted :: [Int]
dataTotalStarted = [1239, 1387, 923, 872, 891, 901, 1049]

dataTotalFinished :: [Int]
dataTotalFinished = [438, 287, 105, 97, 102, 99, 385]

-- finished percent unknown initially
dataTotal :: [TitleStatus Identity]
dataTotal = zipWith (\started finished -> TitleStatus{..}) dataTotalStarted dataTotalFinished

dataUniqueTitlesStarted :: [Int]
dataUniqueTitlesStarted = [42, 41, 36, 35, 35, 36, 42]

dataUniqueTitlesFinished :: [Int]
dataUniqueTitlesFinished = [39, 37, 28, 28, 29, 28, 39]

dataUniqueTitles :: [TitleStatus Identity]
dataUniqueTitles = zipWith (\started finished -> TitleStatus{..}) dataUniqueTitlesStarted dataUniqueTitlesFinished

dataMoviesWatched :: [MoviesWatched Identity]
dataMoviesWatched = zipWith (\total uniqueTitles -> MoviesWatched{..}) dataTotal dataUniqueTitles

dataPauses :: [Pauses Identity]
dataPauses = Pauses <$> [2764, 2874, 1034, 998, 1013, 987, 2764]

newtype Seconds f = Seconds {seconds :: HKD f Int}

dataAvgPauseDuration :: [Seconds Identity]
dataAvgPauseDuration = Seconds <$> [26, 25, 31, 26, 27, 26, 24]

newtype Title a f = Title {title :: HKD f a}

data TitleType = A | B | C | D | E deriving (Data)

dataViews :: [Title TitleType Identity]
dataViews = Title <$> [A, C, A, E, C, E, A]

instance RowShow TitleType where
  rowShow :: TitleType -> Row Text
  rowShow x = pure $ "Title " <> T.pack (show (toConstr x))

instance RowShow (Title TitleType Identity) where
  rowShow :: Title TitleType Identity -> Row Text
  rowShow (Title t) = rowShow t

dataTimeWatched :: [Title TitleType Identity]
dataTimeWatched = Title <$> [B, B, D, D, D, D, B]

dataMostViewed :: [MostViewed Identity]
dataMostViewed = zipWith (\views timeWatched -> MostViewed{..}) dataViews dataTimeWatched

newtype Date f = Date {date :: HKD f Day}

deriving newtype instance Show (Date Identity)

dataDate :: [Date Identity]
dataDate = Date <$> [YearMonthDay 2023 6 x | x <- [1 .. 7]]

instance ToCellData (Date Identity) where
  toCellData (Date d) = pure $ CellValue $ X.CellText $ T.pack $ formatTime defaultTimeLocale "%F" d

deriving newtype instance Show (Pauses Identity)
deriving newtype instance ToCellData (Pauses Identity)
deriving newtype instance Show (Seconds Identity)
deriving newtype instance ToCellData (Seconds Identity)

type family HKD f g where
  HKD Identity g = g
  HKD f g = f g

data TitleStatus f = TitleStatus {started :: HKD f Int, finished :: HKD f Int, finishedPercent :: HKD f Double}
data MoviesWatched f = MoviesWatched {total :: TitleStatus f, uniqueTitles :: TitleStatus f}
newtype Pauses f = Pauses {pauses :: HKD f Int}
data MostViewed f = MostViewed {views :: Title TitleType f, timeWatched :: Title TitleType f}
data Report f = Report
  { date :: Date f
  , moviesWatched :: MoviesWatched f
  , pauses :: Pauses f
  , avgPauseDuration :: Seconds f
  , mostViewed :: MostViewed f
  }

dataReport :: [Report Identity]
dataReport =
  zipWith5
    (\date moviesWatched pauses avgPauseDuration mostViewed -> Report{..})
    dataDate
    dataMoviesWatched
    dataPauses
    dataAvgPauseDuration
    dataMostViewed

rowMoviesTitleStatus :: RowI (TitleStatus Identity) (TitleStatus Ref)
rowMoviesTitleStatus = do
  started <- columnW 7 (.started)
  finished <- columnW 5 (.finished)
  finishedPercent <- columnO (as @Double finished ./ as @Double started)
  pure $ TitleStatus{..}

rowMoviesWatched :: RowI (MoviesWatched Identity) (MoviesWatched Ref)
rowMoviesWatched = do
  total <- with (.total) rowMoviesTitleStatus
  uniqueTitles <- with (.uniqueTitles) rowMoviesTitleStatus
  pure MoviesWatched{..}

rowDate :: RowI (Date Identity) (Date Ref)
rowDate = columnWF 11 (blank .& alignedCenter) id <&> Date

rowPauses :: RowI (Pauses Identity) (Pauses Ref)
rowPauses = columnIO id <&> Pauses

rowAvgPauseDuration :: RowI (Seconds Identity) (Seconds Ref)
rowAvgPauseDuration = columnIO id <&> Seconds

rowTitleType :: RowI (Title TitleType Identity) (Title TitleType Ref)
rowTitleType = columnIO id <&> Title

rowMostViewed :: RowI (MostViewed Identity) (MostViewed Ref)
rowMostViewed = do
  views <- with (.views) rowTitleType
  timeWatched <- with (.timeWatched) rowTitleType
  pure MostViewed{..}

rowReport :: RowI (Report Identity) (Report Ref)
rowReport = do
  date <- with (.date) rowDate
  moviesWatched <- with (.moviesWatched) rowMoviesWatched
  pauses <- with (.pauses) rowPauses
  avgPauseDuration <- with (.avgPauseDuration) rowAvgPauseDuration
  mostViewed <- with (.mostViewed) rowMostViewed
  pure Report{..}

fSum :: ToFormula a => a -> Formula Int
fSum x = fun "SUM" [x]

rowTotal :: [Report Ref] -> Row (Ref Int)
rowTotal dataReport_ = do
  let
    r1 = head dataReport_
    r2 = last dataReport_
  total <- columnO (fSum $ r1.moviesWatched.total.started .: r2.moviesWatched.total.started)
  columnO_ (fSum $ r1.moviesWatched.total.finished .: r2.moviesWatched.total.finished)
  columnO_ (fun "ROUND" [fun "AVERAGE" [r1.moviesWatched.total.finishedPercent .: r2.moviesWatched.total.finishedPercent] :: Formula Double] :: Formula Int)
  columnO_ (fSum $ r1.moviesWatched.uniqueTitles.started .: r2.moviesWatched.uniqueTitles.started)
  columnO_ (fSum $ r1.moviesWatched.uniqueTitles.finished .: r2.moviesWatched.uniqueTitles.finished)
  columnO_ (fun "ROUND" [fun "AVERAGE" [r1.moviesWatched.uniqueTitles.finishedPercent .: r2.moviesWatched.uniqueTitles.finishedPercent] :: Formula Double] :: Formula Int)
  columnO_ (fSum $ r1.pauses.pauses .: r2.pauses.pauses)
  columnO_ (fun "ROUND" [fun "AVERAGE" [r1.avgPauseDuration.seconds .: r2.avgPauseDuration.seconds] :: Formula Double] :: Formula Int)
  columnO_ (fun "MEDIAN" [r1.mostViewed.views.title .: r2.mostViewed.views.title] :: Formula ())
  columnO_ (fun "MEDIAN" [r1.mostViewed.timeWatched.title .: r2.mostViewed.timeWatched.title] :: Formula ())
  pure total

blue :: FormatCell
blue = mkColor (hex @"#FF99CCFF")

alignedCenter :: FCTransform
alignedCenter = horizontalAlignment X.CellHorizontalAlignmentCenter . verticalAlignment X.CellVerticalAlignmentCenter

borderF :: Lens' Border (Maybe BorderStyle) -> FCTransform
borderF f fc =
  fc
    & (X.formattedFormat . X.formatBorder . non X.def . f . non X.def)
      %~ ( \y ->
            y
              & (X.borderStyleColor . non X.def . X.colorARGB %~ const (Just $ toARGB $ hex @"#000000"))
              & (X.borderStyleLine %~ const (Just  X.LineStyleThin))
         )

borders :: FCTransform
borders fc = fc & borderF X.borderStart & borderF X.borderEnd & borderF X.borderBottom & borderF X.borderTop

header :: Ref a -> Int -> Int -> Text -> Sheet (Ref ())
header start colSpan rowSpan name =
  place
    start
    ( columnF
        (blue .& alignedCenter .& (\x -> x{_formattedColSpan = colSpan, _formattedRowSpan = rowSpan}) .& borders)
        (const name)
    )

headerRow :: Ref a -> Int -> Text -> Sheet (Ref ())
headerRow a b = header a b 1

headerRow_ :: Ref a -> Int -> Text -> Sheet ()
headerRow_ a b c = void $ headerRow a b c

header_ :: Ref a -> Int -> Int -> Text -> Sheet ()
header_ a b c d = void $ header a b c d

sheet :: Sheet ()
sheet = do
  start <- mkRef @"A4"
  header_ (start & row -~ 3) 1 3 "Date"
  report <- placeInsRs start dataReport rowReport
  let reportTop = head report
      mw = reportTop.moviesWatched
  totalStarted <- headerRow (mw.total.started & row -~ 1) 1 "Started"
  headerRow_ (totalStarted & col +~ 1) 2 "Finished"
  total <- headerRow (totalStarted & row -~ 1) 3 "Total"

  uniqueTitlesStarted <- headerRow (mw.uniqueTitles.started & row -~ 1) 1 "Started"
  headerRow_ (uniqueTitlesStarted & col +~ 1) 2 "Finished"
  headerRow_ (uniqueTitlesStarted & row -~ 1) 3 "Total"

  mwHeader <- headerRow (total & row -~ 1) 6 "Movies watched"

  pausesHeader <- header (mwHeader & col +~ 6) 1 3 "Pauses"

  header_ (pausesHeader & col +~ 1) 1 3 "Avg.\npauses\nduration"

  let viewsRef = (head report).mostViewed.views.title
  viewsHeader <- headerRow (viewsRef & row -~ 1) 1 "Views"
  headerRow_ (viewsHeader & col +~ 1) 1 "Time\nwatched"
  header_ (viewsHeader & row -~ 2) 2 2 "Most viewed"

  -- header
  -- tl <- placeInsRs start dataMoviesWatched rowMoviesWatched
  let totalStart = (last report).moviesWatched.total.started & row +~ 1
  p <- place totalStart (rowTotal report)
  headerRow_ (p & col -~ 1) 1 "Total"

main :: IO ()
main = writeXlsx "example5.xlsx" [("List 1", sheet)]
