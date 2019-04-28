module Main where

import qualified Diagrams.Backend.SVG as SVG
import           Diagrams.Tests
import           Diagrams.Tests.TwoD

main :: IO ()
main = defaultRunTests SVG SVG.SVG twoDTests
