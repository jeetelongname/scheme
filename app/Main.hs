module Main where

import Parser

main :: IO ()
main = do
  file <- readFile "hello.scm"
  print file
