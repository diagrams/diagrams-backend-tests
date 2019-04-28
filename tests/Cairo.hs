module Main where

import           Diagrams.Backend.Cairo
import           Diagrams.Tests as T
import           Diagrams.Tests.TwoD
import Diagrams.Prelude

cairoText :: TestGroup V2
cairoText = TestGroup "cairo-text"
  [ Test "basic" $ pangoText "pango" # bg grey # rotateBy (1/18)
  ]

main :: IO ()
main = defaultRunTests T.PNG Cairo (twoDTests ++ [cairoText])
