{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Converter
import Convert

main :: IO ()
main = convert Hs TeX