module Main where

import Diagrams.Tests
import Diagrams.Test.SVG
import Diagrams.Test.Cairo
import System.Directory

main = do
   createDirectoryIfMissing False "svg"
   createDirectoryIfMissing False "cairo"
   runTests examples "index.html" $ [cairoTester, svgTester]
