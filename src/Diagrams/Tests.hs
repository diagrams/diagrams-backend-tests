{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Diagrams.Tests
  ( Test(..)
  , TestGroup(..)
  , saveDiagramPng
  , defaultSize
  , DefaultTestFormat (..)
  , wrapper, compact
  , HtmlDisplayOptions (..)
  , saveHtml
  , refOpts
  , renderTests
  , testDia
  , groupDias
  , formatExtension
  ) where

import           Control.Lens        (indexed)
import           Control.Monad.Catch
import           Data.Char           (toLower)
import           System.FilePath
import           System.Process
import           Control.Monad       (when)
import           Data.Text           (Text, pack)
import           Lucid               hiding (with)
import           System.Directory

import           Diagrams.Prelude

-- | The diagram to render along with the name of the test.
data Test v = Test String (Diagram v)

testDia :: IndexedLens' String (Test v) (Diagram v)
testDia f (Test nm dia) = Test nm <$> indexed f nm dia

-- | A named list of tests.
data TestGroup v = TestGroup String [Test v]

groupDias :: IndexedTraversal' String (TestGroup v) (Test v)
groupDias f (TestGroup nm tests) = TestGroup nm <$> traverse (indexed f nm) tests

-- | The format of a backend's output.
data DefaultTestFormat
  = PDF
  | PNG
  | SVG
  | PS
  deriving (Show, Eq)

formatExtension :: DefaultTestFormat -> String
formatExtension = map toLower . show

-- | In order to compare backend results, all outputs are converted to
--   png.
toPng :: DefaultTestFormat -> FilePath -> FilePath -> IO ()
toPng format source dest =
  case format of
    PDF -> do
      _ <- rawSystem "gs"
        [ "-q",  "-dNOPAUSE", "-dBATCH", "-sDEVICE=pngalpha", "-r72", "-dEPSCrop"
        , "-sOutputFile=" ++ dest, source]
      pure ()
    PS  -> do
      _ <- rawSystem "convert" [source, dest]
      pure ()
    PNG -> do
      copyFile source dest
    SVG -> do
      _ <- rawSystem "rsvg-convert" ["--format=png",  source, "-o", dest]
      pure ()

saveDiagramPng
  :: BackendBuild b
  => DefaultTestFormat
  -> b
  -> FilePath
  -> SizeSpec V2 Int
  -> Diagram (V b)
  -> IO ()
saveDiagramPng format b path sz dia = do
  let nativePath = replaceExtension path (formatExtension format)
      pngPath    = replaceExtension path "png"
  saveDiagram b nativePath sz dia
  when (format /= PNG) $ toPng format nativePath pngPath

defaultSize :: SizeSpec V2 Int
defaultSize = dims2D 200 200

renderTests
  :: BackendBuild b
  => DefaultTestFormat
  -> b
  -> FilePath
  -> SizeSpec V2 Int
  -> [TestGroup (V b)]
  -> IO ()
renderTests format b path sz testGroups =
  flip mapM_ testGroups $ \(TestGroup groupName tests) ->
    flip mapM_ tests $ \(Test testName d) ->
      (saveDiagramPng format b (path </> testName) sz d
        `catchAll` \e -> do
          putStrLn $ "error running diagram " <> testName <>
           " for test group " <> groupName <> " with exception "
           <> show e)

------------------------------------------------------------------------

header :: Text -> Html ()
header title = header_ $ do
  div_ [class_ "navbar navbar-dark bg-dark shadow-sm"] $
    div_ [class_ "container d-flex justify-content-between"] $
      a_ [href_ "#", class_ "navbar-brand d-flex align-items-center"] $
        strong_ (toHtml title)

bootstrap :: Html ()
bootstrap = link_ [rel_ "stylesheet", crossorigin_ "anonymous", href_ link, integrity_ sha]
  where
    link = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
    sha = "sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO"

data HtmlDisplayOptions = HtmlDisplayOptions
  { testWidth          :: Int
  , testHeight         :: Int
  , referenceFolder    :: FilePath
  , referenceExtension :: String
  , testFolder         :: FilePath
  , testExtension      :: String
  , testBackendName    :: String
  }

refOpts
  :: String
  -- ^ backend name
  -> FilePath
  -- ^ output path
  -> String
  -- ^ extension
  -> FilePath
  -- ^ reference
  -> HtmlDisplayOptions
refOpts backendName outputPath extension reference = HtmlDisplayOptions
  { testWidth = 200
  , testHeight = 200
  , referenceFolder = reference
  , referenceExtension = "png"
  , testFolder = outputPath
  , testExtension = extension
  , testBackendName = backendName
  }

htmlTestGroup
  :: HtmlDisplayOptions
  -> TestGroup v
  -> Html ()
htmlTestGroup HtmlDisplayOptions {..} (TestGroup suiteName tests) = do
  h2_ (toHtml suiteName)

  div_ [class_ "row py-2"] $ do
    flip mapM_ tests $ \(Test testName _) -> do
      let refSrc = referenceFolder </> testName <.> referenceExtension
          src = testFolder </> testName <.> testExtension
          mkImg s = img_ [src_ (pack s), width_ (pack $ show testWidth), height_ (pack $ show testHeight)]
      div_ [class_ "col-lg-6"] $ do
        h5_ (toHtml testName)
        table_ [class_ "table table-hover"] $
          tr_ $ do
            td_ $ mkImg refSrc
            td_ $ mkImg src

compact
  :: HtmlDisplayOptions
  -> [TestGroup v]
  -> Html ()
compact opts tests = do
  main_ [role_ "main"] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "py-3"] $ do
        mapM_ (htmlTestGroup opts) tests

wrapper :: Text -> Html () -> Html ()
wrapper title body = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      title_ (toHtml title)
      bootstrap
    body_ $ do
      header title
      body

saveHtml :: HtmlDisplayOptions -> [TestGroup v] -> FilePath -> IO ()
saveHtml opts tests outputPath = renderToFile outputPath $ wrapper title (compact opts tests)
  where
    title = "Diagrams backend tests - " <> pack (testBackendName opts)
