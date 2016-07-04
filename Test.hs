{-|
Module      : Test
Description : Test renders for Contextual
Copyright   : (c) Chris Hodapp, 2016
License     : ?
Maintainer  : Hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

So far, this is a lot of scratch code for test renders that I have
done for Contextual.

-}

module Main where

import Control.Monad (forM_)

import qualified Data.Text.Lazy.IO as DT

import Contextual
import qualified CairoBackend as CB
import qualified BlazeBackend as BB
import qualified Graphics.Rendering.Cairo as C 

import qualified System.Random as R

testTriangle :: Node ()
testTriangle = do
  scale (1/3) triangle

-- Not exactly Sierpinski, but an incorrect attempt that looks neat still
notSierpinski :: Node ()
notSierpinski = do
  let xform s = translate (5/6) 0 $ scale (1/3) $ rotate pi s
      pi3 = pi/3
      rep6 = do triangle
                xform rep6
                rotate (1*pi3) $ xform rep6
                rotate (2*pi3) $ xform rep6
                rotate (3*pi3) $ xform rep6
                rotate (4*pi3) $ xform rep6
                rotate (5*pi3) $ xform rep6
  background 0.0 0.0 0.0 1.0
  stroke 0.5 0 0.5 1.0 $ fill 0.4 1.0 0.4 0.8 $ scale 0.5 rep6
  -- TODO: Figure out why the above strokes show up in Blaze backend
  -- but not Cairo

-- Not exactly Sierpinski, but an attempt at it
sierpinski :: Node ()
sierpinski = do
  let xform s = translate (1/3) 0 $ scale (1/3) $ rotate pi s
      pi3 = pi/3
      -- I didn't realize until now that Cairo's triangles were the
      -- wrong size, and I designed this around its coordinates:
      fudge = scale $ sqrt (3/4)
      rep6 = do fudge triangle
                xform rep6
                rotate (1*pi3) $ xform rep6
                rotate (2*pi3) $ xform rep6
                rotate (3*pi3) $ xform rep6
                rotate (4*pi3) $ xform rep6
                rotate (5*pi3) $ xform rep6
  background 0.0 0.0 0.0 1.0
  shiftRGBA 0.8 1.4 3.0 1.3 $ stroke 0 0 0 0 $ fill 1.0 0.3 0.3 0.2 $ rep6

testRandom :: Node ()
testRandom = do
  let r = do
        square
        shiftRGBA 0.8 1.4 2.0 1.0 $ do
          random 0.25
            (translate 0.55 0 $ scale 0.75 $ rotate (pi/3) r)
            (translate 0 (-0.55) $ scale 0.75 $ rotate (-pi/3) r)
          random 0.125
            (do translate (-0.5) 0 $ scale 0.5 $ r
                translate 0 (-0.5) $ scale 0.5 $ r)
            (return ())
  background 0.0 0.0 0.0 1.0
  fill 1.0 0.3 0.3 0.2 $ scale 0.5 r

testHSL :: Node ()
testHSL = do
  let r = do
        square
        shiftHSL (-10) 1.1 1.04 1.07 $ rotate (pi/8) $ translate 0.5 (-0.15) $ scale 0.9 r
  background 0.0 0.0 0.0 1.0
  fill 0.1 0.1 1.0 0.3 $ translate (-0.25) (-0.25) $ scale 0.35 r

testHSL2 :: Node ()
testHSL2 = do
  let part = do
        square
        shiftHSL 6 0.98 0.98 1.07 $ rotate (pi/7) $ translate 0.4 (-0.05) $ scale2 0.9 0.7 $ part
  background 0.98 0.98 0.98 1.0
  stroke 0 0 0 1 $ fill 1.0 0.0 0.0 0.2 $ translate (-0.2) (-0.2) $
    scale 0.5 $ part

test :: Node ()
test = do
  let d = 0.7
      angle = 0.4
  scale 0.75 $ do
    pattern   d    0  angle
    pattern (-d)   0  angle
    pattern   0    d  angle
    pattern   0  (-d) angle

pattern :: Double -> Double -> Double -> Node ()
pattern dx dy angle = do
  scale 0.5 $ do
    square
    translate dx dy $ rotate angle $ shear 0.0 0.2 $ pattern dx dy angle

-- | Probabilistic subdivision of a square
quadtree :: Node ()
quadtree = do
  let r' n = random 0.25 (return ()) n
      d = 0.5
      f = 0.85
      squareR = do
        square
        -- 25% chance of recursing in each quadrant:
        r' $ shiftHSL (12.0) (1/f) f 1.0 $ scale 0.5 $ translate d d $ squareR
        r' $ shiftHSL (-11.0) f (1/f) 1.0 $ scale 0.5 $ translate d (-d) $ squareR
        r' $ shiftHSL (5.0) (1/f) (1/f) 1.0 $ scale 0.5 $ translate (-d) d $ squareR
        r' $ shiftHSL (-8.0) f f 1.0 $ scale 0.5 $ translate (-d) (-d) $ squareR
  background 0 0 0 1
  scale 0.95 $ stroke 0 0 0 1 $ fill 0.6 0.7 1.0 1.0 $ squareR

-- | This is a render for the sake of checking bounds of backends; it
-- contains a square which fills the entire canvas, and another one in
-- front with a slight margin.
testSquare :: Node ()
testSquare = do
  -- Red square fills entire canvas:
  fill 1.0 0.0 0.0 0.5 square
  -- White square, slightly smaller, sits in front:
  scale 0.95 $ fill 1.0 1.0 1.0 0.5 square
  fill 0.0 1.0 0.0 0.5 triangle

main :: IO ()
main = do
  let px = 4000
      py = 4000

  DT.writeFile "quadtree_blaze.svg" $ BB.render (R.mkStdGen 12347) 1e-2 px py quadtree
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12347) 1e-2 px py quadtree
    C.surfaceWriteToPNG surf ("quadtree.png")

  {-
  forM_ [12345, 12346, 12347] $ \seed -> do
    C.withImageSurface C.FormatARGB32 px py $ \surf -> do
      C.renderWith surf $ CB.renderCairo (R.mkStdGen seed) 1e-5 px py testRandom
      C.surfaceWriteToPNG surf ("testRandom" ++ show seed ++ ".png")
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12345) 1e-5 px py testHSL
    C.surfaceWriteToPNG surf ("testHSL.png")
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12345) 1e-5 px py testHSL2
    C.surfaceWriteToPNG surf ("testHSL2.png")
  C.withSVGSurface "testHSL.svg"
    (fromIntegral px) (fromIntegral py) $
    \surf -> C.renderWith surf $ CB.renderCairo (R.mkStdGen 12345) 1e-5 px py testHSL
  DT.writeFile "testHSL2_blaze.svg" $ BB.render (R.mkStdGen 12345) 1e-5 px py testHSL2

  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12345) 1e-5 px py testSquare
    C.surfaceWriteToPNG surf ("testSquare.png")
  DT.writeFile "testSquare_blaze.svg" $ BB.render (R.mkStdGen 12345) 1e-2 px py testSquare

  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12345) 1e-3 px py sierpinski
    C.surfaceWriteToPNG surf ("sierpinski.png")
  DT.writeFile "sierpinski_blaze.svg" $ BB.render (R.mkStdGen 12345) 1e-2 px py sierpinski

  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12345) 1e-3 px py notSierpinski
    C.surfaceWriteToPNG surf ("notSierpinski.png")
  C.withSVGSurface "notSierpinski.svg"
    (fromIntegral px) (fromIntegral py) $
    \surf -> C.renderWith surf $ CB.renderCairo (R.mkStdGen 12345) 1e-3 px py notSierpinski
  DT.writeFile "notSierpinski_blaze.svg" $ BB.render (R.mkStdGen 12345) 1e-2 px py notSierpinski
  -}
