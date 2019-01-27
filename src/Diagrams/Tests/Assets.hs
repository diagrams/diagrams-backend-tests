{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Diagrams.Tests.Assets where

import           Data.ByteString              (ByteString)
import           Data.FileEmbed
import           Diagrams.Prelude             (V2 (..))
import           System.FilePath              ((</>))

#ifdef CABAL
import qualified Paths_diagrams_backend_tests
import           System.IO.Unsafe
#else
import qualified Language.Haskell.TH.Syntax   as TH
#endif

assetsPath :: FilePath
assetsPath =
#ifdef CABAL
  unsafePerformIO $ (</> "assets") <$> Paths_diagrams_backend_tests.getDataDir
#else
  $(TH.lift =<< makeRelativeToProject "assets")
#endif

jpgImgPath :: FilePath
jpgImgPath = assetsPath </> "cat.jpg"

jpgImgSize :: V2 Int
jpgImgSize = V2 320 231

pngImgPath :: FilePath
pngImgPath = assetsPath </> "cat.png"

pngImgSize :: V2 Int
pngImgSize = V2 300 214

imgBS :: ByteString
imgBS = $(makeRelativeToProject "assets/cat.jpg" >>= embedFile)

imgAlphaBS :: ByteString
imgAlphaBS = $(makeRelativeToProject "assets/cat.png" >>= embedFile)
