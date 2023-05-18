{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Converter
import Test (convert)

main :: IO ()
main = convert Hs TeX