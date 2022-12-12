module Main where

import Parser

main :: IO ()
main = do
  file <- readFile "hello.scm"
  let schemeValues = getParser readScheme file
  print schemeValues
