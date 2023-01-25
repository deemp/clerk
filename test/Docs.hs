{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- FOURMOLU_ENABLE -}

module Main (main) where

import Control.Monad (when)
import GHC.IO.Exception (ExitCode (..))
import System.Exit (exitFailure, exitSuccess)
import System.Process.Typed (runProcess, shell)

main :: IO ()
main = do
  print "Converting README"
  s1 <-
    runProcess $
      shell $
        unlines
          [ "lima hs2md -f example/app/Main.hs"
          , "touch README.tmp"
          , "cat README/Intro.md >> README.tmp"
          , "printf \"\n\" >> README.tmp"
          , "cat example/app/Main.hs.md >> README.tmp && rm example/app/Main.hs.md"
          , "printf \"\n\" >> README.tmp"
          , "cat README/Outro.md >> README.tmp"
          , "mv README.tmp README.md"
          ]
  when (s1 /= ExitSuccess) (print "Failed to generate README.md. Exiting ..." >> exitFailure)
  exitSuccess