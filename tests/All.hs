module Main where

import           Diagrams.Test.Cairo
import           Diagrams.Test.PGF
import           Diagrams.Test.Postscript
import           Diagrams.Test.Rasterific
import           Diagrams.Test.SVG
import           Diagrams.Tests
import           System.Directory

main = do
  let mkdir = createDirectoryIfMissing False
  mkdir "cairo"
  mkdir "pgf"
  mkdir "postscript"
  mkdir "rasterific"
  mkdir "svg"

  runTests examples "all-index.html" $
    [cairoTester, pgfTester, postscriptTester, rasterificTester, svgTester]
