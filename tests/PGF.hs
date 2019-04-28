module Main where

import           Diagrams.Backend.PGF
import           Diagrams.Tests
import           Diagrams.Tests.TwoD

main :: IO ()
main = defaultRunTests PDF PGF twoDTests
