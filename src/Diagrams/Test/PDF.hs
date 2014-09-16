module Diagrams.Test.PDF (pdfTester) where

import           Diagrams.Backend.Pdf
import           Diagrams.Backend.Pdf.CmdLine
import           Diagrams.Prelude             hiding ((<.>))
import           Diagrams.Tests
import           Graphics.PDF
import           System.FilePath              ((<.>), (</>))
import           System.Process
import           Text.Html                    as H hiding ((</>))

pdfTester :: (String, Test -> IO Html)
pdfTester =
  ( "pdf"
  , \ (Test nm dig) -> do
      let docRect = PDFRect 0 0 200 200
          pdfOpts = PdfOptions (Dims 200 200)
      runPdf (name nm "pdf") (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) docRect $ do
              page1 <- addPage Nothing
              drawWithPage page1 $ renderDia Pdf pdfOpts dig
      rawSystem "convert" [name nm "pdf", name nm "png"]
      return $ H.image ! [ src (name nm "png") ]
  )
 where
  name nm ext = prefix </> nm <.> ext
  prefix = "pdf"
