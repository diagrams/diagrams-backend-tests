module Diagrams.Test.Postscript (postscriptTester) where

import Diagrams.Backend.Postscript
import Diagrams.Prelude hiding ((<.>))
import Diagrams.Tests
import System.FilePath ((</>), (<.>))
import System.Process
import Text.Html as H hiding ((</>))

postscriptTester :: (String, Test -> IO Html)
postscriptTester =
  ( "postscript"
  , \ (Test nm dig) -> do
      renderDia Postscript
        (PostscriptOptions (name nm "eps") (Dims 200 200) EPS)
        dig
      rawSystem "epstopdf" [name nm "eps"]
      rawSystem "convert" [name nm "pdf", name nm "png"]
      return $ H.image ! [ src (name nm "png") ]
  )
 where
  name nm ext = prefix </> nm <.> ext
  prefix = "postscript"
