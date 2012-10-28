module Main where

import qualified Data.ByteString.Lazy as BS
import           Diagrams.Backend.SVG
import           Diagrams.Prelude hiding (D, (<.>))
import           Diagrams.Tests
import           System.Directory
import           System.FilePath ((</>), (<.>))
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import           Text.Html as H hiding ((</>))

main = do
   -- all output is put into the SVG directory
   createDirectoryIfMissing False prefix
   -- and run the tests to generate the SVG examples
   runTests (examples) "svg-index.html" $ [("SVG", \ (Test nm dig) -> do
     let svg = renderDia SVG (SVGOptions (Dims 200 200)) dig
     BS.writeFile (name nm) (renderSvg svg)
     return
         $ H.image ! [ src (name nm) ]
     )]
  where
     name nm = prefix </> nm <.> "svg"
     prefix = "svg"
