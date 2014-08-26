module Diagrams.Test.Postscript (postscriptTester) where

import Codec.Picture (readImage, DynamicImage(..), Image(..), PixelRGBA8)
import Codec.Picture.Types (dropAlphaLayer, promoteImage)
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
            -- Make both images RGB8.
            (Right  i1, Right i2) -> compareImages (syncImgage i1) (syncImgage i2)
          syncImgage img = case img of
            ImageRGBA8 i -> ImageRGB8 $ dropAlphaLayer i
            ImageYA8 i   -> ya82rgb8 i
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
  ya82rgb8 ya8 = ImageRGB8 img
    where
      img  = dropAlphaLayer img'
      img' = promoteImage ya8 :: (Image PixelRGBA8)
