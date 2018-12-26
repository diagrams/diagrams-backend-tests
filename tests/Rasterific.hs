module Main where

import           Diagrams.Backend.Rasterific
import           Diagrams.Tests
import           Diagrams.Tests.TwoD

main :: IO ()
main = defaultRunTests "rasterific" PNG Rasterific
