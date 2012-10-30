module Main where

import System.Directory

import Diagrams.Tests
import Diagrams.Test.Cairo

main = do
   createDirectoryIfMissing False "cairo"
   runTests (examples) "cairo-index.html" $ [cairoTester]
