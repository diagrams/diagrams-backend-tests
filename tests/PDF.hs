module Main where

import           System.Directory

import           Diagrams.Test.PDF
import           Diagrams.Tests

main = do
   createDirectoryIfMissing False "pdf"
   runTests (examples) "pdf-index.html" $ [pdfTester]
