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

import Contextual
import CairoBackend
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
  scale 0.5 rep6

notSierpinski_render :: Int -> Int -> C.Render ()
notSierpinski_render px py = do
  -- TODO: Fix below (don't actually need random generator for
  -- deterministic grammars):
  let rg = R.mkStdGen 12345
  -- Open a context (not strictly needed, I don't think, but it
  -- may help if we ever need to compose anything):
  C.save
  preamble px py True
  renderCairo rg 0.5e-3 (ColorRGBA 0.4 1.0 0.4 0.8) notSierpinski
  C.restore

-- Not exactly Sierpinski, but an attempt at it
sierpinski :: Node ()
sierpinski = do
  let xform s = translate (1/3) 0 $ scale (1/3) $ rotate pi s
      pi3 = pi/3
      rep6 = do triangle
                xform rep6
                rotate (1*pi3) $ xform rep6
                rotate (2*pi3) $ xform rep6
                rotate (3*pi3) $ xform rep6
                rotate (4*pi3) $ xform rep6
                rotate (5*pi3) $ xform rep6
  shiftRGBA 0.8 1.4 3.0 1.3 $ rep6

sierpinski_render :: Int -> Int -> C.Render ()
sierpinski_render px py = do
  -- TODO: Fix below (don't actually need random generator for
  -- deterministic grammars):
  let rg = R.mkStdGen 12345
  -- Open a context (not strictly needed, I don't think, but it
  -- may help if we ever need to compose anything):
  C.save
  preamble px py True
  renderCairo rg 0.5e-3 (ColorRGBA 1.0 0.3 0.3 0.2) sierpinski
  C.restore

testRandom :: Node ()
testRandom = do
  square
  shiftRGBA 0.8 1.4 2.0 1.0 $ do
    random 0.25
      (translate 0.55 0 $ scale 0.75 $ rotate (pi/3) testRandom)
      (translate 0 (-0.55) $ scale 0.75 $ rotate (-pi/3) testRandom)
    random 0.125
      (do translate (-0.5) 0 $ scale 0.5 $ testRandom
          translate 0 (-0.5) $ scale 0.5 $ testRandom)
      (return ())

testRandom_render :: Int -> Int -> Int -> C.Render ()
testRandom_render px py seed = do
  let rg = R.mkStdGen seed
  C.save
  preamble px py True
  renderCairo rg 1e-5 (ColorRGBA 1.0 0.3 0.3 0.2) $ scale 0.5 $ testRandom
  C.restore  

testHSL :: Node ()
testHSL = do
  square
  shiftHSL (-10) 1.1 1.05 1.07 $ rotate (pi/8) $ translate 0.5 (-0.15) $ scale 0.9 $ testHSL

testHSL_render :: Int -> Int -> C.Render ()
testHSL_render px py = do
  let rg = R.mkStdGen 12345
  C.save
  preamble px py True
  renderCairo rg 1e-5 (ColorRGBA 0.0 0.0 1.0 0.2) $
    translate (-0.25) (-0.25) $
    scale 0.35 $
    testHSL
  C.restore  

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

main :: IO ()
main = do
  let px = 1000
      py = 1000
  forM_ [12345, 12346, 12347] $ \seed -> do
    C.withImageSurface C.FormatARGB32 px py $ \surf -> do
      C.renderWith surf $ testRandom_render px py seed
      C.surfaceWriteToPNG surf ("testRandom" ++ show seed ++ ".png")
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ testHSL_render px py
    C.surfaceWriteToPNG surf ("testHSL.png")
  C.withSVGSurface "testHSL.svg"
    (fromIntegral px) (fromIntegral py) $ \surf ->
    do
      C.renderWith surf $ testHSL_render px py
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ sierpinski_render px py
    C.surfaceWriteToPNG surf ("sierpinski.png")
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ notSierpinski_render px py
    C.surfaceWriteToPNG surf ("notSierpinski.png")
