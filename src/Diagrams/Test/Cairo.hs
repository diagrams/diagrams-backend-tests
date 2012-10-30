module Diagrams.Test.Cairo (cairoTester) where

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude hiding (D, (<.>))
import Diagrams.Tests
import System.FilePath ((</>),(<.>))
import Text.Html as H hiding ((</>))

cairoTester :: (String, Test -> IO Html)
cairoTester =
  ( "cairo"
  , \ (Test nm dig) -> do
      fst $ renderDia Cairo
              (CairoOptions (name nm) (Dims 200 200) PNG)
              dig
      return $ H.image ! [ src (name nm) ]
  )
 where
  name nm = prefix </> nm <.> "png"
  prefix = "cairo"
