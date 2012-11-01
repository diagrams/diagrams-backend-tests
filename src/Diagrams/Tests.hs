{-# LANGUAGE FlexibleContexts, RankNTypes, NoMonomorphismRestriction #-}

module Diagrams.Tests
        ( Test(..)
        , runTests
        , examples
        ) where

import           Diagrams.Core.Points
import           Diagrams.Prelude
import           Diagrams.TwoD.Text
import           Text.Html (Html, tr, concatHtml, th, (<<), valign, toHtml, (!), body, table, renderHtml, src, td, bgcolor)
import qualified Text.Html as H

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
                arc (tau/4 :: Rad) (4 * tau / 7 :: Rad)
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
                beside (20 & 30) (circle 1 # fc orange) (circle 1.5 # fc purple)
                        # showOrigin
        , Test "juxtaposing2" $
                let d1 = circle 1 # fc red
                    d2 = square 1 # fc blue
                in  (d1 ||| d2) ||| strutX 3 ||| ( d1
                                                   ===
                                                   d2  )

        , Test "freeze" $
               (square 1 ||| square 1 # freeze # scale 2
                         ||| circle 1 # freeze # scaleX 3
               ) # lw 0.03

        , Test "line-attributes" $
               let path = fromVertices [0 & 0, 1 & 0.3, 2 & 0, 2.2 & 0.3] # lw 0.1
               in pad 1.1 . centerXY . vcat' with { sep = 0.1 }
                  $ map (path #)
                  [ lineCap LineCapButt   . lineJoin LineJoinMiter
                  , lineCap LineCapRound  . lineJoin LineJoinRound
                  , lineCap LineCapSquare . lineJoin LineJoinBevel
                  , dashing [0.1,0.2,0.3,0.1] 0
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
               let text' s t = text t # fontSize s <> strutY (s * 1.3)
               in pad 1.1 . centerXY $
                    text' 10 "Hello" # italic
                    === text' 5 "there"  # bold # font "freeserif"
                    === text' 3 "world"  # fc green

        , Test "text-transforms" $
               let eff = text "F" <> square 1 # lw 0
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
                # lw 0.05
                # clipBy (square 3.2 # rotateBy (1/10))

        , Test "alpha-color" $

               let colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
               in  hcat' with { catMethod = Distrib, sep = 1 }
                     (zipWith fcA colors (repeat (circle 1)))

        , Test "opacity1" $
               let s c     = square 1 # fc c
                   reds    = (s darkred ||| s red) === (s pink ||| s indianred)
               in  hcat' with { sep = 1 } . take 4 . iterate (opacity 0.7) $ reds

        ]

poly_example = (poly1 ||| strutX 1 ||| poly2) # lw 0.05
  where
          poly1 = polygon with { polyType   = PolyRegular 13 5
                               , polyOrient = OrientV }
          poly2 = polygon with { polyType   = PolyPolar (repeat (1/40 :: CircleFrac))
                                       (take 40 $ cycle [2,7,4,6]) }
