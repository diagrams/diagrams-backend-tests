module Main where

import Data.Maybe
import Text.Html
import Data.Functor
import System.Environment

import Diagrams.Tests
import Diagrams.Test.PGF
import Diagrams.Backend.PGF.Surface
import System.Directory

main = do
  createDirectoryIfMissing False "pgf"
  runTests examples "pgf-index.html" [pgfTester]

tests :: IO [(String, Test Double -> IO Html)]
tests = do
  args <- concatMap (mapMaybe parseChar) <$> getArgs

  return $ if null args
             then [pgfTester]
             else args

parseChar :: Char -> Maybe (String, Test Double -> IO Html)
parseChar 'p' = Just $ pgfTester' plaintexSurface
parseChar 'l' = Just $ pgfTester' latexSurface
parseChar 'c' = Just $ pgfTester' contextSurface
parseChar _   = Nothing

