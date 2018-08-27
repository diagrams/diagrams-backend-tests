module Main where

import Data.Maybe
import Text.Html
import Data.Functor
import System.Environment

import Control.Lens
import Diagrams.Backend
import Diagrams.Tests
import Linear.V2
-- import Diagrams.Test.PGF
import Diagrams.Backend.PGF
import Diagrams.Backend.PGF.Surface
import System.Directory

main = do
  createDirectoryIfMissing False "pgf"
  runTests examples "pgf-index.html" [pgfTester latexSurface]

tests :: IO [(String, Test V2 -> IO Html)]
tests = do
  args <- concatMap (mapMaybe parseChar) <$> getArgs

  return $ if null args
             then [pgfTester latexSurface]
             else args

parseChar :: Char -> Maybe (String, Test V2 -> IO Html)
parseChar 'p' = Just $ pgfTester plaintexSurface
parseChar 'l' = Just $ pgfTester latexSurface
parseChar 'c' = Just $ pgfTester contextSurface
parseChar _   = Nothing

pgfTester :: Surface -> (String, Test V2 -> IO Html)
pgfTester surf = (nm, mkRunTest nm (mkOptions defaultSize & surface .~ surf) PDF)
  where nm = "pgf" -- -" ++ surf ^. command
