module Diagrams.Test.Postscript (postscriptTester) where

import Codec.Picture (readImage, DynamicImage(..))
import Codec.Picture.Types (promoteImage)
import Diagrams.Compare
import Diagrams.Backend.Postscript
import Diagrams.Prelude hiding ((<.>))
import Diagrams.Tests
import System.FilePath ((</>), (<.>))
import System.Process
import Text.Html as H hiding ((</>))
import Text.Printf (printf)

postscriptTester :: (String, Test -> IO Html)
postscriptTester =
  ( "postscript"
  , \ (Test nm dig) -> do
      renderDia Postscript
        (PostscriptOptions (name nm "eps") (Dims 200 200) EPS)
        dig
      rawSystem "epstopdf" [name nm "eps"]
      rawSystem "convert" [name nm "pdf", name nm "png"]
      -- The generated image.
      img <- readImage $ name nm "png"
      -- The reference image.
      ref <- readImage $ "ref/" ++ nm ++ ".png"
      -- m: is mean squared error, p: is peak signal to noise ration.
      let (m,p) = case (ref, img) of
            (Left _, _) -> error "Image 1 not read"
            (_, Left _) -> error "Image 2 not read"
            -- If either image does not have an alpha layer then add one.
            (Right  i1, Right i2) -> compareImages (addAlpha i1) (addAlpha i2)
          addAlpha img= case img of
            ImageRGB8 i -> ImageRGBA8 $ promoteImage i
            otherwise -> img
          -- figure and figCaption are new to Html5 and are implemented
          -- in Diagrams.Tests.
          cap = figCaption (toHtml $ "mse: " ++ printf "%5.3e" m 
                                             ++ ", psnr: " 
                                             ++ printf "%4.1f" p
                                             ++ " db")
      return $ figure $ H.image ! [ src (name nm "png") ] +++ cap
  )
 where
  name nm ext = prefix </> nm <.> ext
  prefix = "postscript"
