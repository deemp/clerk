{-# LANGUAGE AllowAmbiguousTypes #-}

module Clerk.ColorTyped where

import Clerk.Format (ToARGB (toARGB))
import Clerk.Symbols (FromChars, ToChars)
import Data.Data (Proxy (..))
import Data.Kind (Constraint)
import Data.Text (Text)
import qualified Data.Text as T
import Fcf (Eval, Exp, If, Map, Uncurry)
import Fcf.Class.Foldable (All, Or)
import Fcf.Data.List (Span)
import GHC.TypeLits

-- color

-- data RGBA = RGBA {_red :: Word8, _green :: Word8, _blue :: Word8, _alpha :: Word8}

-- >>>:t P

-- newtype Color = Color RGBA
-- instance Show Color where
--   show :: Color -> String
--   show (Color (RGBA {})) = "a"

newtype Color = Color {_color :: Text}

instance ToARGB Color where
  toARGB (Color _color) = _color

instance Show Color where
  show = T.unpack . _color

-- hex

type family LEQ1 (b :: Ordering) :: Bool where
  LEQ1 LT = True
  LEQ1 EQ = True
  LEQ1 GT = False

type family LEQ (b :: Char) (c :: Char) :: Bool where
  LEQ b c = LEQ1 (CmpChar b c)

data ExpLEQPair :: (Char, Char) -> Exp Bool
type instance Eval (ExpLEQPair '(c1, c2)) = LEQ c1 c2

data ExpBool :: Bool -> Exp Bool
type instance Eval (ExpBool c) = c

type family CheckList (cs :: [(Char, Char)]) :: Bool where
  CheckList cs = Eval (All ExpBool (Eval (Map ExpLEQPair cs)))

data ExpCheckList :: [(Char, Char)] -> Exp Bool
type instance Eval (ExpCheckList cs) = CheckList cs

type family CheckChar2 (cs :: [[(Char, Char)]]) :: Bool where
  CheckChar2 cs = Eval (Or (Eval (Map ExpCheckList cs)))

type family CheckChar1 (c :: Char) where
  CheckChar1 c = CheckChar2 [['( '0', c), '(c, '9')], ['( 'a', c), '(c, 'f')], ['( 'A', c), '(c, 'F')]]

data ExpCheckChar :: Char -> Exp Bool
type instance Eval (ExpCheckChar c) = CheckChar1 c

type family CheckNonEmpty (s :: [a]) :: Bool where
  CheckNonEmpty (c : cs) = True
  CheckNonEmpty cs = False

type family Constraint' :: Constraint where
  Constraint' = ()

type family Color3 (pref :: [Char]) (suff :: [Char]) :: (Symbol, Constraint) where
  Color3 pref suff =
    If
      (CheckNonEmpty suff)
      ( TypeError
          ( 'Text "Error parsing a color code.\n"
              :<>: ('Text "  Parsed: " :<>: 'Text (FromChars pref) :<>: 'Text "\n")
              :<>: ('Text "  Failed to parse: " :<>: 'Text (FromChars suff) :<>: 'Text "\n")
              :<>: 'Text "  Expected one of these characters: \"0123456789abcdefABCDEF\""
          )
      )
      '(FromChars pref, Constraint')

data Color3Exp :: [Char] -> [Char] -> Exp (Symbol, Constraint)
type instance Eval (Color3Exp a b) = Color3 a b

type family Color2 (cs :: [Char]) :: (Symbol, Constraint) where
  Color2 cs = Eval (Uncurry Color3Exp (Eval (Span ExpCheckChar cs)))

type family ColorChecked1 (c :: [Char]) :: (Symbol, Constraint) where
  ColorChecked1 ('#' : cs) = Color2 cs
  ColorChecked1 cs = TypeError ('Text "The color literal must start with a \"#\"")

type family ColorChecked (c :: Symbol) :: (Symbol, Constraint) where
  ColorChecked c = ColorChecked1 (ToChars c)

hex :: forall c constr s. (KnownSymbol c, KnownSymbol s, '(s, constr) ~ ColorChecked c) => Color
hex = Color (T.pack (symbolVal (Proxy @s)))

-- >>> p = hex @"#-abAcd"
-- Error parsing a color code.
--   Parsed:
--   Failed to parse: -abAcd
--   Expected one of these characters: "0123456789abcdefABCDEF"
-- In the expression: hex @"#-abAcd"
-- In an equation for `p': p = hex @"#-abAcd"

-- rgba

class ToRGBA' c where
  toRGBA' :: c -> RGBA r g b a

data RGBA (r :: Nat) (g :: Nat) (b :: Nat) (a :: Nat) where
  RGBA :: (r <= 255, g <= 255, b <= 255, a <= 255) => RGBA r g b a

type ValidRGBA r g b a =
  ( KnownNat r
  , r <= 255
  , KnownNat g
  , g <= 255
  , KnownNat b
  , b <= 255
  , KnownNat a
  , a <= 255
  )

-- type family ToWord8 (c1 :: Char) (c2 :: Char) :: Word8 where
--   ToWord8 c1 c2 =

-- type family ParseRGBA (cs :: [Char]) :: RGBA' a b c d where
--     ParseRGBA [c1,c2,c3,c4,c5,c6,c7,c8] = RGBA' 2 3 4 5
