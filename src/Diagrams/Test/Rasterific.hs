module Diagrams.Test.Rasterific (rasterificTester) where

import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude hiding (D, (<.>))
import           Diagrams.Tests
import           System.FilePath ((</>),(<.>))
import           Text.Html as H hiding ((</>))

rasterificTester :: (String, Test -> IO Html)
rasterificTester =
  ( "rasterific"
  , \ (Test nm dig) -> do
      renderRasterific (name nm) (Dims 200 200) 100 dig
      return $ H.image ! [ src (name nm) ]
  )
 where
   name nm = prefix </> nm <.> "png"
   prefix = "rasterific"
