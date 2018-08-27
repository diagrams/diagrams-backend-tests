{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Diagrams.Tests
  ( Test(..)
  , figure, figCaption
  , mkRunTest
  , defaultRunTest
  , runTests
  , examples
  , defaultSize
  , DefaultTestFormat (..)
  ) where

import           System.Process
import           Data.Typeable
import           Diagrams.Prelude     hiding (connect)
-- import           Diagrams.Attributes
-- import           Diagrams.TwoD.Attributes
-- import           Diagrams.TwoD.Path hiding (clipBy)
-- import           Diagrams.TwoD.Path.Unboxed (uStroke)
-- import           Diagrams.TwoD.Combinators
import           Diagrams.Combinators
-- import           Diagrams.Measure
-- import Diagrams.Types
import Data.Char (toLower)
import Geometry
import System.FilePath
import System.Environment
import Diagrams.Backend
import           Diagrams.TwoD.Text
import           Text.Html            (Html, bgcolor, body, concatHtml,
                                       renderHtml, src, table, td, th, toHtml,
                                       tr, valign, (!), (<<))
import qualified Text.Html            as H

-- | The diagram to render along with the name of the test.
data Test v = Test String (Diagram v)

figure :: Html -> Html
figure = H.tag "FIGURE"

figCaption :: Html -> Html
figCaption = H.tag "FIGCAPTION"

-- | 'runTests' generates an HTML page which contains the executed
--   tests.  You need to provide the tests to run (typically
--   'examples'), the name of the file to generate, and a list of
--   backend plugins: each backend should provide its name as well as
--   a method for creating an HTML fragment that displays the
--   generated image.

data DefaultTestFormat
  = PDF
  | PNG
  | SVG
  | PS

defaultSize :: SizeSpec V2 Int
defaultSize = dims2D 200 200

defaultRunTest
  :: (Typeable b, V b ~ v, BackendBuild b)
  => DefaultTestFormat -> b -> (String, Test v -> IO Html)
defaultRunTest fmt b = (tName, mkRunTest tName (mkOptionsFor b defaultSize) fmt)
  where tName = map toLower . show $ typeOf b

mkRunTest
  :: (V b ~ v, BackendBuild b)
  => String -> Options b -> DefaultTestFormat -> Test v -> IO Html
mkRunTest tName opts fmt (Test nm dia) = do

  let mkName ext = tName </> nm <.> ext

  case fmt of
    PDF -> do
      saveDiagram' (mkName "tex") opts dia
      saveDiagram' (mkName "pdf") opts dia
      rawSystem "gs" [ "-q",  "-dNOPAUSE", "-dBATCH", "-sDEVICE=pngalpha", "-r72", "-dEPSCrop"
                          , "-sOutputFile=" ++ mkName "png", mkName "pdf" ]
      return $ figure $ H.image ! [ src (mkName "png") ]
    PS  -> do
      saveDiagram' (mkName "eps") opts dia
      rawSystem "convert" ["-alpha", "on", mkName "eps", mkName "png32"]
      return $ figure $ H.image ! [ src (mkName "png") ]
    PNG -> do
      saveDiagram' (mkName "png") opts dia
      pure (error "don't look at me")
      return $ figure $ H.image ! [ src (mkName "png") ]
    SVG -> do
      saveDiagram' (mkName "svg") opts dia
      return $ figure $ H.image ! [ src (mkName "svg") ]

runTests :: [Test v] -> String -> [(String, Test v -> IO Html)] -> IO ()
runTests tests name backends = do
  let (backendNames, execs) = unzip backends
      header = tr . concatHtml $
                      [ th << "test name"
                      , th << "expected output"
                      ]
                      ++
                      map ((th <<) . (++ " output")) backendNames
  result_rows <- sequence
          [ do testHtmls <- mapM ($t) execs
               let golden = figure $ H.image ! [src ("ref/" ++ nm ++ ".png")]
               return . tr . concatHtml $
                 [ td ! [valign "top", bgcolor "#eeeeee"]  $ toHtml nm
                 , td ! [valign "top"] $ golden
                 ]
                 ++
                 map td testHtmls
          | t@(Test nm _) <- tests
          ]
  let doc = body
          $ table
          $ concatHtml (header : result_rows)
  writeFile name $ renderHtml doc

-- | List of default examples.
examples :: [Test V2]
examples = concat [basicTests, textTests, fillRuleTests, gradientTests]

basicTests :: [Test V2]
basicTests =
  [ Test "square1" $ square 1
  , Test "circle1" $ circle 1
  , Test "circle-square" $
          circle 1 ||| square 1
  , Test "2-circles" $
          circle 0.5 <> unitCircle
  , Test "ellipse" $
          unitCircle # scaleX 0.5 # rotateBy (1/6)
  , Test "arc" $
          arc (xDir # rotate (tau/4 @@ rad)) (9 * tau / 28 @@ rad)
  , Test "Pre-defined-shapes" $
          square 1 ||| rect 0.3 0.5 ||| eqTriangle 1 ||| roundedRect 0.7 0.4 0.1
  , Test "circle-hrule-circle" $
          circle 1 ||| hrule 2 ||| circle 1
  , Test "poly-example" $
          poly_example
  , Test "star-polygon" $
          star (StarSkip 3) (regPoly 13 1) # stroke
  , Test "star-skip" $
          stroke (star (StarSkip 2) (regPoly 8 1))
                 ||| strutX 1
                 ||| stroke (star (StarSkip 3) (regPoly 8 1))
  , Test "superimposing" $
          circle 1 <> square (sqrt 2)
  , Test "superimposing-color" $
          mconcat [ circle 0.1 # fc green
                  , eqTriangle 1 # scale 0.4 # fc yellow
                  , square 1 # fc blue
                  , circle 1 # fc red
                  ]
  , Test "juxtaposing1" $
          beside (V2 20 30) (circle 1 # fc orange) (circle 1.5 # fc purple)
                  # showOrigin
  , Test "juxtaposing2" $
          let d1 = circle 1 # fc red
              d2 = square 1 # fc blue
          in  (d1 ||| d2) ||| strutX 3 ||| ( d1
                                             ===
                                             d2  )

  , Test "line-attributes" $
         let path = fromVertices [mkP2 0 0, mkP2 1 0.3, mkP2 2 0, mkP2 2.2 0.3] # lwG 0.1
         in pad 1.1 . centerXY . vsep 0.1
            $ map (path #)
            [ lineCap LineCapButt   . lineJoin LineJoinMiter
            , lineCap LineCapRound  . lineJoin LineJoinRound
            , lineCap LineCapSquare . lineJoin LineJoinBevel
            , dashingG [0.1,0.2,0.3,0.1] 0
            ]
  ]

textTests :: [Test V2]
textTests =
  [ Test "text-basic" $
         text "Hello world!" <> rect 8 1

  , Test "text-alignment" $
         let pt = circle 0.1 # fc red

             t1 = pt <> topLeftText         "top left"   <> rect 8 1
             t2 = pt <> baselineText        "baseline"   <> rect 8 1
             t3 = pt <> alignedText 0.7 0.5 "(0.7, 0.5)" <> rect 8 1

             d1 =/= d2 = d1 === strutY 2 === d2

         in  t1 =/= t2 =/= t3

  , Test "text-attributes" $
         let text' s t = text t # fontSizeG s <> strutY (s * 1.3)
         in pad 1.1 . centerXY $
              text' 10 "Hello" # italic
              === text' 5 "there"  # bold # font "freeserif"
              === text' 3 "world"  # fc green

  , Test "text-transforms" $
         let eff = text "F" <> square 1 # lwG 0
             ts  = [ scale (1/2), id, scale 2, scaleX 2, scaleY 2
                   , scale (-1), scaleX (-1), scaleY (-1)
                   ]

         in  pad 1.1 . hcat . map (eff #) $ ts

  , Test "text-transforms-normal" $
         let eff = text "F" # fontSize (normalized 0.2) <> square 1 # lwG 0
             ts  = [ scale (1/2), id, scale 2, scaleX 2, scaleY 2
                   , scale (-1), scaleX (-1), scaleY (-1)
                   ]

         in  pad 1.1 . hcat . map (eff #) $ ts
  ]

fillRuleTests :: [Test V2]
fillRuleTests =
  [ Test "ring" $
         let ring = circle 3 <> circle 2 :: Path V2 Double

         in  stroke ring # fc purple # fillRule EvenOdd # pad 1.1

  , Test "fill-rules" $
         let loopyStar = fc red
                       . mconcat
                       . map (cubicSpline True)
                       . pathVertices
                       . star (StarSkip 3)
                       $ regPoly 7 1
         in   loopyStar # fillRule EvenOdd
              ||| strutX 1
              ||| loopyStar # fillRule Winding

  , Test "clip" $
          square 3
          # fc green
          # lwG 0.05
          -- # clipBy (square 3.2 # rotateBy (1/10))
          # clip (square 3.2 # rotateBy (1/10))

  , Test "clip-multi" $ square 2 # fc orange
                                 # clip (square 1)
                                 # clip (square 1 # rotateBy (1/8))
                                 -- # clipBy (square 1)
                                 -- # clipBy (square 1 # rotateBy (1/8))

  , Test "clip-stacked" $
      let p = square 500 # alignBL
          bg = p # fc blue
          sq1 = p # fc green
          sq2 = p # fc red
                  # opacity 0.5
                  -- # clipBy (square 200 # alignBL # translate (r2 (200, 100)))
                  # clip (square 200 # alignBL # translate (r2 (200, 100)))
          sq3 = p # fc red
                  -- # clipBy (rect 250 100 # alignBL # translate (r2 (150, 350)))
                  # clip (rect 250 100 # alignBL # translate (r2 (150, 350)))
      in sq3
      -- <> (sq2 <> sq1) # clipBy (square 200 # alignBL # translate (r2 (100, 200)))
      <> (sq2 <> sq1) # clip (square 200 # alignBL # translate (r2 (100, 200)))
      <> bg

  , Test "alpha-color" $

         let colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
         in  hsepEven 1 (zipWith fcA colors (repeat (circle 1)))

  , Test "opacity1" $
         let s c     = square 1 # fc c
             reds    = (s darkred ||| s red) === (s pink ||| s indianred)
         in  hsep 1 . take 4 . iterate (opacity 0.7) $ reds

  , Test "text-opacity" $ pad 1.1 . centerXY $
         opacity 0.2 $ rect 8 1 # lwG 0.2 <> text "hello"

  , Test "group-opacity" $
         let circles = (circle 1 <> circle 1 # translateX 1) # fc red # lw none
         in  hsep 1 [circles # opacity 0.3, circles # groupOpacity 0.3]

  , Test "fat" $
         unitCircle # lwG 0.3 # scaleX 2 # pad 1.3

  -- , Test "connect" $ connect_example

  -- , Test "fill-line" $
  --        uStroke (fromVertices [origin, mkP2 0 2, mkP2 3 3, mkP2 4 1])
  --          # fc blue

  -- , Test "fill-loop" $
  --        strokeLoop (fromVertices [origin, mkP2 0 2, mkP2 3 3, mkP2 4 1] # closeLine)
  --          # fc blue

  , Test "line-loop" $
      fc green . stroke $
         (fromVertices
           [origin, mkP2 0 2, mkP2 3 3, mkP2 4 1]
             # rotateBy (1/12)
             # mapLoc closeLine
             # Path . pure . mapLoc wrapLoop)
         <>
         (fromVertices
           [origin, mkP2 0 2, mkP2 3 3, mkP2 4 1]
             # Path . pure . mapLoc wrapLine)
  ]

gradientTests :: [Test V2]
gradientTests =
  [ Test "triangle-miter" $
         triangle 1   # fc green # rotateBy (1/5)
         <>
         square   1.2 # fc white # lwG 0

  , Test "linear-gradient" linearGradient_example

  , Test "radial-gradient" radialGradient_example
  ]

asPath :: Path v n -> Path v n
asPath = id

poly_example = (poly1 ||| strutX 1 ||| poly2) # lwG 0.05
  where
          poly1 = polygon (with & polyType   .~ PolyRegular 13 5
                                & polyOrient .~ OrientV
                          )
          poly2 = polygon (with & polyType   .~ PolyPolar
                                                  (repeat (1/40 @@ turn))
                                                  (take 40 $ cycle [2,7,4,6])
                          )

-- data Corner = NW | NE | SW | SE
--   deriving (Typeable, Eq, Ord, Show)
-- instance IsName Corner

-- connect n1 n2
--   = withName n1 $ \b1 ->
--     withName n2 $ \b2 ->
--       atop ((location b1 ~~ location b2) # lc red # lwG 0.05)

-- squares =  (s # named NW ||| s # named NE)
--        === (s # named SW ||| s # named SE)
--   where s = square 1 # lwG 0.05

-- d = hcat' (with & sep .~ 0.5) (zipWith (.>>) [0::Int ..] (replicate 5 squares))

-- pairs = [ ((0::Int) .> NE, (2::Int) .> SW)
--         , ((1::Int) .> SE, (4::Int) .> NE)
--         , ((3::Int) .> NW, (3::Int) .> SE)
--         , ((0::Int) .> SE, (1::Int) .> NW)
--         ]

-- connect_example = d # applyAll (map (uncurry connect) pairs)

linearGradient_example :: Diagram V2
linearGradient_example = hsep 0.25 [sq1, sq2, sq3] where

  stops = [(red, 0), (white, 0.5), (gold, 1)]
  gradient = mkLinearGradient stops (P2 (-0.5) 0) (P2 0.5 0)

  sq1 = square 1 # fillTexture gradient

  gradient2 = gradient
    & gradientSpreadMethod .~ GradRepeat
    & gradientStart        .~ P2 (-0.1) 0
    & gradientEnd          .~ P2 0.1 0
  sq2 = square 1 # fillTexture gradient2

  gradient3 = gradient2 & gradientSpreadMethod .~ GradReflect
  sq3 = square 1 # fillTexture gradient3


radialGradient_example :: Diagram V2
radialGradient_example = rg
  where
    gradient = mkRadialGradient ([(gray,1), (purple,1)]) origin 0.5
                         -- (0 ^& 0) 0.1 (0 ^& 0) 0.5
                         -- GradPad
    sq1 = square 1 # fillTexture  gradient

    gradient2 = gradient
      & gradientSpreadMethod .~ GradRepeat
      & gradientRadius0      .~ 0.1
      & gradientRadius1      .~ 0.3
    sq2 = square 1 # fillTexture gradient2

    gradient3 = gradient
      & gradientSpreadMethod .~ GradReflect
      & gradientRadius0      .~ 0.1
      & gradientRadius1      .~ 0.2
    sq3 = square 1 # fillTexture gradient3

    rg = hsep 0.25 [sq1, sq2, sq3]

