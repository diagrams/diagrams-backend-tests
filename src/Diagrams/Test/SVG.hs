module Diagrams.Test.SVG (svgTester) where

import           Codec.Picture (readImage, DynamicImage(..), Image(..), PixelRGBA8)
import           Codec.Picture.Types (dropAlphaLayer, promoteImage)
import           Diagrams.Compare
import qualified Data.ByteString.Lazy as BS
import           Diagrams.Backend.SVG
import           Diagrams.Prelude hiding (D, (<.>))
import           Diagrams.Tests
import           System.FilePath ((</>), (<.>))
import           System.Process
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import           Text.Html as H hiding ((</>))
import           Text.Printf (printf)

svgTester :: (String, Test -> IO Html)
svgTester =
  ( "SVG"
  , \ (Test nm dig) -> do
      let svg = renderDia SVG (SVGOptions (Dims 200 200) Nothing) dig
      BS.writeFile (name nm "svg") (renderSvg svg)
      rawSystem "convert" ["-background", "none", name nm "svg", name nm "png32"]
      -- The generated image.
      img <- readImage $ name nm "png32"
      -- The reference image.
      ref <- readImage $ "ref/" ++ nm ++ ".png"
      -- m: is mean squared error, p: is peak signal to noise ration.
      let (m,p) = case (ref, img) of
            (Left _, _) -> error "Image 1 not read"
            (_, Left _) -> error "Image 2 not read"
            (Right  i1, Right i2) -> compareImages (addAlpha i1) i2
          addAlpha img= case img of
            ImageRGB8 i -> ImageRGBA8 $ promoteImage i
            otherwise -> img
          -- figure and figCaption are new to Html5 and are implemented
          -- in Diagrams.Tests.
          cap = figCaption (toHtml $ "mse: " ++ printf "%5.3e" m 
                                             ++ ", psnr: " 
                                             ++ printf "%4.1f" p
                                             ++ " db")
      return $ figure $ H.image ! [ src (name nm "svg") ] +++ cap
  )
 where
  name nm ext = prefix </> nm <.> ext
  prefix = "svg"
