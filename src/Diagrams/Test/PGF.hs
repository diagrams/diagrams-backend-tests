module Diagrams.Test.PGF (pgfTester, pgfTester') where

import           Codec.Picture        (DynamicImage (..), readImage)
import           Codec.Picture.Types  (promoteImage)

import           Diagrams.Backend.PGF
import           Diagrams.Compare
import           Diagrams.Prelude
import           Diagrams.Tests

import           System.FilePath      ((<.>), (</>))
import           System.Process
import           Text.Html            as H hiding ((</>))
import           Text.Printf

pgfTester' :: Surface -> (String, Test Double -> IO Html)
pgfTester' surf =
  ( "pgf - " ++ _command surf
  , \ (Test nm d) -> do
      renderPGF (name nm "pdf") (dims2D 200 200) surf d

      -- rawSystem "convert" [name nm "pdf", name nm "png"]
      rawSystem "gs" [ "-q",  "-dNOPAUSE", "-dBATCH", "-sDEVICE=pngalpha", "-r72", "-dEPSCrop"
                     , "-sOutputFile=" ++ name nm "png", name nm "pdf" ]

      img <- readImage $ name nm "png"
      ref <- readImage $ "ref/" ++ nm ++ ".png"

      let (m,p) = case (ref, img) of
            (Left _, _)           -> error "Image 1 not read"
            (_, Left _)           -> error "Image 2 not read"
            (Right  i1, Right i2) -> compareImages (addAlpha i1) (addAlpha i2)

          addAlpha img = case img of
            ImageRGB8 i -> ImageRGBA8 $ promoteImage i
            _           -> img

          cap = figCaption . toHtml $ "mse: " ++ printf "%5.3e" m
                                              ++ ", psnr: "
                                              ++ printf "%4.1f" p
                                              ++ " db"
      return $ figure $ H.image ! [ src (name nm "png") ] +++ cap

  )
 where
   name nm ext = "pgf" </> nm <.> ext

pgfTester = pgfTester' latexSurface

