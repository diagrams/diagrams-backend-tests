{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Diagrams.Tests.CmdLine
  ( TestOpts (..)
  , testOpts
  ) where

#ifdef CABAL
import qualified Paths_diagrams_backend_tests
#else
import qualified Data.FileEmbed
import qualified Language.Haskell.TH.Syntax   as TH
#endif

import           Data.Char                    (toLower)
import           Data.Maybe
import           Data.Text                    (pack)
import           Options.Generic
import           System.FilePath

-- | Internal optphrse-generic data type for cmdline options.
data TestOptsG w = TestOptsG
  { ref    :: w ::: Maybe FilePath <?> "The path the to reference images"
  , output :: w ::: Maybe FilePath <?> "The folder to save the test output"
  , group  :: w ::: [FilePath] <?> "The group of tests to build"
  } deriving Generic

instance ParseRecord (TestOptsG Wrapped)

-- | Postprocessed options.
data TestOpts = TestOpts
  { referenceFolder :: FilePath
  , outputFolder    :: FilePath
  , testGroups      :: [String]
  }

testOpts :: String -> IO TestOpts
testOpts name = do
  let name' = fmap toLower name
  args <- unwrapRecord ("diagrams-" <> pack name' <> " backend tests")
#ifdef CABAL
  -- If we're using cabal return the path to the data directory for the
  -- compiled library.
  rdd <- (</> "ref") <$> Paths_diagrams_backend_tests.getDataDir
#else
  -- Otherwise just link to the local source version
  let rdd = $(TH.lift =<< Data.FileEmbed.makeRelativeToProject "ref")
#endif
  pure TestOpts
    { referenceFolder = fromMaybe rdd (ref args)
    , outputFolder    = fromMaybe (name' <> "-tests") (output args)
    , testGroups      = group args
    }

