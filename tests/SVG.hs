module Main where

import Diagrams.Tests
import Diagrams.Test.SVG
import System.Directory

main = do
   createDirectoryIfMissing False "svg"
   runTests (examples) "svg-index.html" $ [svgTester]
