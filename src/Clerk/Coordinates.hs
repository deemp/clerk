{-# LANGUAGE OverloadedRecordDot #-}

module Clerk.Coordinates where

import Codec.Xlsx (ColumnIndex (..), RowIndex (..))
import qualified Codec.Xlsx as X
import Data.Char (chr, isAlpha, isUpper, ord)
import Data.Default (Default (def))
import Data.List (foldl')
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)

-- | Index of an input
newtype InputIndex = InputIndex Int deriving newtype (Num, Show, Integral, Enum, Real, Ord, Eq, Default)

-- | Coords of a cell
data Coords = Coords
  { _col :: ColumnIndex
  , _row :: RowIndex
  , _coordsWorksheetName :: String
  , _coordsWorkbookPath :: FilePath
  , _inputIndex :: InputIndex
  }
  deriving stock (Generic, Show)

deriving anyclass instance Default RowIndex
deriving anyclass instance Default ColumnIndex
deriving anyclass instance Default Coords

-- | A lens for @RowIndex@
row :: forall a. CoordsLike a => Lens' a RowIndex
row = lens getter setter
 where
  getter :: a -> RowIndex
  getter (toCoords -> Coords{_row}) = _row
  setter :: a -> RowIndex -> a
  setter (toCoords -> Coords{_row, ..}) f = fromCoords $ Coords{_row = f, ..}

-- | A lens for @RowIndex@ as an @Int@
rowI :: forall a. CoordsLike a => Lens' a Int
rowI = lens getter setter
 where
  getter :: a -> Int
  getter (toCoords -> Coords{_row}) = _row.unRowIndex
  setter :: a -> Int -> a
  setter (toCoords -> Coords{_row, ..}) f = fromCoords $ Coords{_row = RowIndex f, ..}

-- | A lens for @ColumnIndex@
col :: forall a. CoordsLike a => Lens' a ColumnIndex
col = lens getter setter
 where
  getter :: a -> ColumnIndex
  getter (toCoords -> Coords{_col}) = _col
  setter :: a -> ColumnIndex -> a
  setter (toCoords -> Coords{_col, ..}) f = fromCoords $ Coords{_col = f, ..}

-- | A lens for @ColumnIndex@ as an @Int@
colI :: forall a. CoordsLike a => Lens' a Int
colI = lens getter setter
 where
  getter :: a -> Int
  getter (toCoords -> Coords{_col}) = _col.unColumnIndex
  setter :: a -> Int -> a
  setter (toCoords -> Coords{_col, ..}) f = fromCoords $ Coords{_col = ColumnIndex f, ..}

-- | Convertible to 'Coords'
class ToCoords a where
  toCoords :: a -> Coords

-- | Convertible from 'Coords'
class FromCoords a where
  fromCoords :: Coords -> a

type CoordsLike a = (FromCoords a, ToCoords a)

instance ToCoords Coords where
  toCoords :: Coords -> Coords
  toCoords = id

instance FromCoords Coords where
  fromCoords :: Coords -> Coords
  fromCoords = id

instance FromCoords (X.RowIndex, X.ColumnIndex) where
  fromCoords :: Coords -> (X.RowIndex, X.ColumnIndex)
  fromCoords Coords{..} = (_row, _col)

-- Can't define 'ToCoords' for a pair because miss some 'Coords' data.

instance Num Coords where
  (+) :: Coords -> Coords -> Coords
  (+) Coords{_row = r1, _col = c1, ..} Coords{_row = r2, _col = c2} = Coords{_row = r1 + r2, _col = c1 + c2, ..}
  (*) :: Coords -> Coords -> Coords
  (*) Coords{_row = r1, _col = c1, ..} Coords{_row = r2, _col = c2} = Coords{_row = r1 * r2, _col = c1 * c2, ..}
  (-) :: Coords -> Coords -> Coords
  (-) Coords{_row = r1, _col = c1, ..} Coords{_row = r2, _col = c2} = Coords{_row = r1 - r2, _col = c1 - c2, ..}
  abs :: Coords -> Coords
  abs Coords{..} = Coords{_row = abs _row, _col = abs _col, ..}
  signum :: Coords -> Coords
  signum Coords{..} = Coords{_row = signum _row, _col = signum _col, ..}
  fromInteger :: Integer -> Coords
  fromInteger x = def{_row = fromIntegral x, _col = fromIntegral x}

alphabetSize :: Int
alphabetSize = 26

-- | Translate 'ColumnIndex' into letters
--
-- @
-- >>> toLetters <$> [1, 26, 27, 52, 78]
-- ["A","Z","AA","AZ","BZ"]
--
-- @
toLetters :: ColumnIndex -> T.Text
toLetters (fromIntegral -> x) = f "" (x - 1)
 where
  new :: Int -> T.Text -> T.Text
  new cur acc = T.pack [chr (ord 'A' + (cur `mod` alphabetSize))] <> acc
  f :: T.Text -> Int -> T.Text
  f acc cur = if cur `div` alphabetSize > 0 then f (new cur acc) (cur `div` alphabetSize - 1) else new cur acc

-- | Translate a column address into a number
fromLetters :: T.Text -> Either ColumnIndexTranslationError ColumnIndex
fromLetters x
  | alphaLength /= T.length x =
      Left
        ( ColumnIndexTranslationError
            { _atPosition = alphaLength
            , _character = T.index x alphaLength
            }
        )
  | otherwise = Right (unsafeColumnIndexFromLetters x)
 where
  alphaLength = T.length (T.takeWhile (\y -> isAlpha y && isUpper y) x)

data ColumnIndexTranslationError = ColumnIndexTranslationError {_atPosition :: Int, _character :: Char}

instance Show ColumnIndexTranslationError where
  show :: ColumnIndexTranslationError -> String
  show (ColumnIndexTranslationError{..}) = "Column index contains an invalid character at position: " <> show _atPosition

unsafeColumnIndexFromString :: String -> ColumnIndex
unsafeColumnIndexFromString x = fromIntegral $ foldl' (\res c -> res * alphabetSize + (ord c - ord 'A' + 1)) 0 x

unsafeColumnIndexFromLetters :: T.Text -> ColumnIndex
unsafeColumnIndexFromLetters = unsafeColumnIndexFromString . T.unpack
