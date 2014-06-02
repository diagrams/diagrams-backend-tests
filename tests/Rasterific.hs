module Main where

import System.Directory

import Diagrams.Tests
import Diagrams.Test.Rasterific

main = do
   createDirectoryIfMissing False "rasterific"
   runTests (examples) "rasterific-index.html" $ [rasterificTester]
