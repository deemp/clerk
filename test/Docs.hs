{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant bracket" #-}

module Main (main) where

import Converter (hsToMd)
import Data.Default (def)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Turtle (MonadIO (liftIO), readTextFile, sh, writeTextFile)

main :: IO ()
main = do
  T.putStrLn "Converting README"
  sh $ liftIO $ do
    intro <- readTextFile "README/Intro.md"
    let examplesPaths = (\(x :: Int) -> fromString $ "app/Example" <> show x <> ".hs") <$> [1 .. 4]
        convertExamples = ((T.pack . hsToMd def . T.unpack) <$>) . readTextFile <$> examplesPaths
    examples <- sequenceA convertExamples
    outro <- readTextFile "README/Outro.md"
    writeTextFile "README.md" (T.intercalate "\n" ([intro] <> examples <> [outro]))