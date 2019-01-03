module Main where

import           Diagrams.Backend.Cairo
import           Diagrams.Tests as T
import           Diagrams.Tests.TwoD

main :: IO ()
main = defaultRunTests "cairo" T.PNG Cairo
