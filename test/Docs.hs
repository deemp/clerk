{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad (when)
import Data.String.Interpolate (i)
import Data.Text.IO qualified as Text
import GHC.IO.Exception (ExitCode (..))
import System.Exit (exitFailure, exitSuccess)
import Turtle (Alternative (empty), Text, shellStrictWithErr)

main :: IO ()
main = do
  Text.putStrLn "Converting README"
  let appendNewline :: Text = [i|printf "\n" >> README.tmp|]
  (exitCode, _, _) <-
    shellStrictWithErr
      [i|
        lima hs2md -f app/Example1.hs -f app/Example2.hs
        touch README.tmp
        cat README/Intro.md >> README.tmp
        #{appendNewline}
        cat app/Example1.hs.md >> README.tmp && rm app/Example1.hs.md
        #{appendNewline}
        cat app/Example2.hs.md >> README.tmp && rm app/Example2.hs.md
        #{appendNewline}
        cat README/Outro.md >> README.tmp
        mv README.tmp README.md
      |]
      empty
  when (exitCode /= ExitSuccess) (Text.putStrLn "Failed to generate README.md. Exiting ..." >> exitFailure)
  exitSuccess