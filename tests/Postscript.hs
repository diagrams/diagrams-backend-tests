module Main where

import System.Directory

import Diagrams.Tests
import Diagrams.Test.Postscript

main = do
   createDirectoryIfMissing False "postscript"
   runTests (examples) "postscript-index.html" $ [postscriptTester]
