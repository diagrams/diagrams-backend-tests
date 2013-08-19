module Main where

import System.Directory

import Diagrams.Tests
import Diagrams.Test.Pdf

main = do
   createDirectoryIfMissing False "pdf"
   runTests (examples) "pdf-index.html" $ [pdfTester]
