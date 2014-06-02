{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}

module Diagrams.Tests
        ( Test(..)
        , runTests
        , examples
        ) where

import           Data.Typeable
import           Diagrams.Core.Points
import           Diagrams.Prelude     hiding (connect)
import           Diagrams.TwoD.Text
import           Text.Html            (Html, bgcolor, body, concatHtml,
                                       renderHtml, src, table, td, th, toHtml,
                                       tr, valign, (!), (<<))
import qualified Text.Html            as H

-----------------------------------------------------------------------

data Test = Test
        String -- ^ the name of the test
        (forall canvas .
                ( Renderable (Path R2) canvas
                , Renderable Text      canvas
                , Backend canvas R2
                ) => Diagram canvas R2
        ) -- ^ and the diagram

-----------------------------------------------------------------------

-- | 'runTests' generates an HTML page which contains the executed
--   tests.  You need to provide the tests to run (typically
--   'examples'), the name of the file to generate, and a list of
--   backend plugins: each backend should provide its name as well as
--   a method for creating an HTML fragment that displays the
--   generated image.

runTests ::  [Test] -> String -> [(String, Test -> IO Html)] -> IO ()
runTests tests name backends = do
        let (backendNames, execs) = unzip backends
            header = tr . concatHtml $
                            [ th << "(name)"
                            , th << "expected output"
                            ]
                            ++
                            map ((th <<) . (++ " output")) backendNames
        result_rows <- sequence
                [ do testHtmls <- mapM ($t) execs
                     let golden = H.image ! [src ("ref/" ++ nm ++ ".png")]
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

-- ^ list of cannonical examples.
examples :: [Test]
examples =
        [ Test "square1" $ square 1
        , Test "circle1" $ circle 1
        , Test "circle-square" $
                circle 1 ||| square 1
        , Test "2-circles" $
                circle 0.5 <> unitCircle
        , Test "ellipse" $
                unitCircle # scaleX 0.5 # rotateBy (1/6)
        , Test "arc" $
                arc (tau/4 @@ rad) (4 * tau / 7 @@ rad)
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
                circle 1 `atop` square (sqrt 2)
        , Test "superimposing-color" $
                mconcat [ circle 0.1 # fc green
                        , eqTriangle 1 # scale 0.4 # fc yellow
                        , square 1 # fc blue
                        , circle 1 # fc red
                        ]
        , Test "juxtaposing1" $
                beside (20 ^& 30) (circle 1 # fc orange) (circle 1.5 # fc purple)
                        # showOrigin
        , Test "juxtaposing2" $
                let d1 = circle 1 # fc red
                    d2 = square 1 # fc blue
                in  (d1 ||| d2) ||| strutX 3 ||| ( d1
                                                   ===
                                                   d2  )

        , Test "line-attributes" $
               let path = fromVertices [0 ^& 0, 1 ^& 0.3, 2 ^& 0, 2.2 ^& 0.3] # lwG 0.1
               in pad 1.1 . centerXY . vcat' (with & sep .~ 0.1)
                  $ map (path #)
                  [ lineCap LineCapButt   . lineJoin LineJoinMiter
                  , lineCap LineCapRound  . lineJoin LineJoinRound
                  , lineCap LineCapSquare . lineJoin LineJoinBevel
                  , dashingG [0.1,0.2,0.3,0.1] 0
                  ]

        , Test "text-basic" $
               text "Hello world!" <> rect 8 1

        , Test "text-alignment" $
               let pt = circle 0.1 # fc red

                   t1 = pt <> topLeftText         "top left"   <> rect 8 1
                   t2 = pt <> baselineText        "baseline"   <> rect 8 1
                   t3 = pt <> alignedText 0.7 0.5 "(0.7, 0.5)" <> rect 8 1

                   d1 =/= d2 = d1 === strutY 2 === d2

               in  t1 =/= t2 =/= t3

        , Test "text-attributes" $
               let text' s t = text t # fontSize (Global s) <> strutY (s * 1.3)
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

        , Test "ring" $
               let ring :: Path R2
                   ring = circle 3 <> circle 2

               in  stroke ring # fc purple # fillRule EvenOdd # pad 1.1

        , Test "fill-rules" $
               let loopyStar = fc red
                             . mconcat . map (cubicSpline True)
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
                # clipBy (square 3.2 # rotateBy (1/10))

        , Test "clip-stacked" $
            let p = stroke $ square 500 # alignBL
                bg = p # fc blue
                sq1 = p # fc green
                sq2 = p # fc red
                        # opacity 0.5
                        # clipBy (square 200 # alignBL # translate (r2 (200, 100)))
                sq3 = p # fc red
                        # clipBy (rect 250 100 # alignBL # translate (r2 (150, 350)))
            in sq3
            <> (sq2 <> sq1) # clipBy (square 200 # alignBL # translate (r2 (100, 200)))
            <> bg

        , Test "alpha-color" $

               let colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
               in  hcat' (with & catMethod .~ Distrib & sep .~ 1)
                     (zipWith fcA colors (repeat (circle 1)))

        , Test "opacity1" $
               let s c     = square 1 # fc c
                   reds    = (s darkred ||| s red) === (s pink ||| s indianred)
               in  hcat' (with & sep .~ 1) . take 4 . iterate (opacity 0.7) $ reds

        , Test "text-opacity" $ pad 1.1 . centerXY $
               opacity 0.2 $ rect 8 1 # lwG 0.2 <> text "hello"

        , Test "fat" $
               unitCircle # lwG 0.3 # scaleX 2 # pad 1.3

        , Test "connect" $ connect_example

        , Test "fill-line" $
               strokeLine (fromVertices [origin, 0 ^& 2, 3 ^& 3, 4 ^& 1])
                 # fc blue

        , Test "fill-loop" $
               strokeLoop (fromVertices [origin, 0 ^& 2, 3 ^& 3, 4 ^& 1] # closeLine)
                 # fc blue

        , Test "line-loop" $
               fc green $
               stroke $
               trailLike ((fromVertices [origin, 0 ^& 2, 3 ^& 3, 4 ^& 1] # rotateBy (1/12) # closeLine # wrapLoop) `at` origin)
               <>
               trailLike ((fromVertices [origin, 0 ^& 2, 3 ^& 3, 4 ^& 1] # wrapLine) `at` origin)

        , Test "triangle-miter" $
               triangle 1   # fc green # rotateBy (1/5)
               <>
               square   1.2 # fc white # lwG 0

        , Test "linear-gradient" $ linearGradient_example

        , Test "radial-gradient" $ radialGradient_example
        ]

poly_example = (poly1 ||| strutX 1 ||| poly2) # lwG 0.05
  where
          poly1 = polygon (with & polyType   .~ PolyRegular 13 5
                                & polyOrient .~ OrientV
                          )
          poly2 = polygon (with & polyType   .~ PolyPolar
                                                  (repeat (1/40 @@ turn))
                                                  (take 40 $ cycle [2,7,4,6])
                          )

data Corner = NW | NE | SW | SE
  deriving (Typeable, Eq, Ord, Show)
instance IsName Corner

connect n1 n2
  = withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      atop ((location b1 ~~ location b2) # lc red # lwG 0.05)

squares =  (s # named NW ||| s # named NE)
       === (s # named SW ||| s # named SE)
  where s = square 1 # lwG 0.05

d = hcat' (with & sep .~ 0.5) (zipWith (|>) [0::Int ..] (replicate 5 squares))

pairs = [ ((0::Int) .> NE, (2::Int) .> SW)
        , ((1::Int) .> SE, (4::Int) .> NE)
        , ((3::Int) .> NW, (3::Int) .> SE)
        , ((0::Int) .> SE, (1::Int) .> NW)
        ]

connect_example = d # applyAll (map (uncurry connect) pairs)

linearGradient_example = lg
  where
    stops = mkStops [(red, 0, 1), (white, 0.5, 1), (gold, 1, 1)]
    gradient = mkLinearGradient stops ((-0.5) ^& 0) (0.5 ^& 0) GradPad
    sq1 = square 1 # fillTexture  gradient
    sq2 = square 1 # fillTexture (gradient & _LG . lGradSpreadMethod .~ GradRepeat
                                          & _LG . lGradStart .~ (-0.1) ^& 0
                                          & _LG . lGradEnd .~ 0.1 ^& 0)
    sq3 = square 1 # fillTexture (gradient & _LG . lGradSpreadMethod .~ GradReflect
                                          & _LG . lGradStart .~ (-0.1) ^& 0
                                          & _LG . lGradEnd .~ 0.1 ^& 0)
    lg = hcat' (with & sep .~ 0.25) [sq1, sq2, sq3]


radialGradient_example = rg
  where
    gradient = mkRadialGradient (mkStops [(gray,0,1), (purple,1,1)])
                         (0 ^& 0) 0.1 (0 ^& 0) 0.5
                         GradPad
    sq1 = square 1 # fillTexture  gradient
    sq2 = square 1 # fillTexture (gradient & _RG . rGradSpreadMethod .~ GradRepeat
                                           & _RG . rGradRadius0 .~ 0.1
                                           & _RG . rGradRadius1 .~ 0.3)
    sq3 = square 1 # fillTexture (gradient & _RG . rGradSpreadMethod .~ GradReflect
                                          & _RG . rGradRadius0 .~ 0.1
                                           & _RG . rGradRadius1 .~ 0.2)
    rg = hcat' (with & sep .~ 0.25) [sq1, sq2, sq3]
