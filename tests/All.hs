module Main where

import Diagrams.Test.Cairo
import Diagrams.Test.Postscript
import Diagrams.Test.SVG
import Diagrams.Test.PDF
import Diagrams.Tests
import System.Directory

main = do
   createDirectoryIfMissing False "svg"
   createDirectoryIfMissing False "cairo"
   createDirectoryIfMissing False "postscript"
   createDirectoryIfMissing False "pdf"

   runTests examples "all-index.html" $ [cairoTester, svgTester, postscriptTester, pdfTester]
