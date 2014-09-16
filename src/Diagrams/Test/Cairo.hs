module Diagrams.Test.Cairo (cairoTester) where

import Codec.Picture (readImage, DynamicImage(..))
import Codec.Picture.Types (promoteImage)
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude hiding (D, (<.>))
import Diagrams.Tests
import Diagrams.Compare
import System.FilePath ((</>),(<.>))
import Text.Html as H hiding ((</>))
import Text.Printf (printf)

cairoTester :: (String, Test Double -> IO Html)
cairoTester =
  ( "cairo"
  , \ (Test nm dig) -> do
      fst $ renderDia Cairo
              (CairoOptions (name nm) (Dims 200 200) PNG False)
              dig
      img <- readImage $ name nm
      ref <- readImage $ "ref/" ++ nm ++ ".png"
      let (m,p) = case (ref, img) of
            (Left _, _) -> error "Image 1 not read"
            (_, Left _) -> error "Image 2 not read"
            (Right  i1, Right i2) -> compareImages (addAlpha i1) (addAlpha i2)
          addAlpha img= case img of
            ImageRGB8 i -> ImageRGBA8 $ promoteImage i
            otherwise -> img
          cap = figCaption (toHtml $ "mse: " ++ printf "%5.3e" m 
                                             ++ ", psnr: " 
                                             ++ printf "%4.1f" p
                                             ++ " db")
      return $ figure $ H.image ! [ src (name nm) ] +++ cap
  )
 where
  name nm = prefix </> nm <.> "png"
  prefix = "cairo"


