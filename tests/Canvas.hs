module Main where

import Text.Html as H
import Diagrams.Prelude hiding (D)
import Diagrams.Backend.Canvas
import Diagrams.Tests
import System.Directory


main = do
   -- all output is put into the canvas directory
   createDirectoryIfMissing False "canvas"
   -- and run the tests to generate the html5 canvas examples
   runTests (examples) "canvas-index.html" $ [("canvas", \ (Test nm dig) -> do
        renderDia Canvas (CanvasOptions (Dims 200 200)) dig
        return
         $ tag "iframe" ! [ src $ name nm
                          , H.height 210
                          , H.width "210"
                          , intAttr "frameborder"  0
                          , intAttr "marginheight" 0
                          , intAttr "scrolling"    0
                          ] << noHtml
      )]
  where
     name nm = "canvas/" ++ nm ++ ".html"
