module Diagrams.Test.SVG (svgTester) where

import qualified Data.ByteString.Lazy as BS
import           Diagrams.Backend.SVG
import           Diagrams.Prelude hiding (D, (<.>))
import           Diagrams.Tests
import           System.FilePath ((</>), (<.>))
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import           Text.Html as H hiding ((</>))

svgTester :: (String, Test -> IO Html)
svgTester =
  ( "SVG"
  , \ (Test nm dig) -> do
      let svg = renderDia SVG (SVGOptions (Dims 200 200) Nothing) dig
      BS.writeFile (name nm) (renderSvg svg)
      return $ figure $ H.image ! [ src (name nm) ]
  )
 where
  name nm = prefix </> nm <.> "svg"
  prefix = "svg"
