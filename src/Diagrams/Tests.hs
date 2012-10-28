{-# LANGUAGE FlexibleContexts, RankNTypes, NoMonomorphismRestriction #-}

module Diagrams.Tests
        ( Test(..)
        , runTests
        , examples
        ) where

import           Diagrams.Core.Points
import           Diagrams.Prelude
import           Text.Html (Html, tr, concatHtml, th, (<<), valign, toHtml, (!), body, table, renderHtml, src, td, bgcolor)
import qualified Text.Html as H

-----------------------------------------------------------------------

data Test = Test
        String -- ^ the name of the test
        (forall canvas .
                ( Renderable (Path R2) canvas
                , Backend canvas R2
                ) => Diagram canvas R2
        ) -- ^ and the diagram

-----------------------------------------------------------------------

-- | 'runTests' generates an HTML page which contains the executed tests.
-- You need to provide the tests to run (typically 'examples'),
-- the name of the file to generate, and generating an HTML fragment
-- that displays your image.

runTests ::  [Test] -> String -> (Test -> IO Html) -> IO ()
runTests tests name exec = do
        let header = tr
                $ concatHtml [ th << "(name)"
                             , th << "expected output"
                             , th << "diagram under test"
                             ]
        result_rows <- sequence
                [ do inside <- exec t
                     let golden = H.image ! [src ("ref/" ++ nm ++ ".png")]
                     return
                        $ tr
                        $ concatHtml [ td ! [valign "top", bgcolor "#eeeeee"]  $ toHtml nm
                                     , td ! [valign "top"] $ golden
                                     , td $ inside
                                     ]
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
        ]


poly_example = (poly1 ||| strutX 1 ||| poly2) # lw 0.05
  where
          poly1 = polygon with { polyType   = PolyRegular 13 5
                               , polyOrient = OrientV }
          poly2 = polygon with { polyType   = PolyPolar (repeat (1/40 :: CircleFrac))
                                       (take 40 $ cycle [2,7,4,6]) }

