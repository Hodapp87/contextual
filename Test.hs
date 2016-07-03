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

notSierpinski_render :: Int -> Int -> C.Render ()
notSierpinski_render px py = do
  -- TODO: Fix below (don't actually need random generator for
  -- deterministic grammars):
  let rg = R.mkStdGen 12345
  -- Open a context (not strictly needed, I don't think, but it
  -- may help if we ever need to compose anything):
  C.save
  CB.preamble px py
  CB.renderCairo rg 1e-3 $ notSierpinski
  C.restore

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

sierpinski_render :: Int -> Int -> C.Render ()
sierpinski_render px py = do
  -- TODO: Fix below (don't actually need random generator for
  -- deterministic grammars):
  let rg = R.mkStdGen 12345
  -- Open a context (not strictly needed, I don't think, but it
  -- may help if we ever need to compose anything):
  C.save
  CB.preamble px py
  CB.renderCairo rg 1e-3 $ sierpinski
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
  CB.preamble px py
  CB.renderCairo rg 1e-5 $ do
    background 0.0 0.0 0.0 1.0
    fill 1.0 0.3 0.3 0.2 $ 
      scale 0.5 $ testRandom
  C.restore  

testHSL :: Node ()
testHSL = do
  square
  shiftHSL (-10) 1.1 1.04 1.07 $ rotate (pi/8) $ translate 0.5 (-0.15) $ scale 0.9 $ testHSL

testHSL_render :: Int -> Int -> C.Render ()
testHSL_render px py = do
  let rg = R.mkStdGen 12345
  C.save
  CB.preamble px py
  CB.renderCairo rg 1e-5 $ do
    background 0.0 0.0 0.0 1.0
    fill 0.1 0.1 1.0 0.3 $ translate (-0.25) (-0.25) $
      scale 0.35 $
      testHSL
  C.restore  

testHSL2 :: Node ()
testHSL2 = do
  let part = do
        square
        shiftHSL 6 0.98 0.98 1.07 $ rotate (pi/7) $ translate 0.4 (-0.05) $ scale2 0.9 0.7 $ part
  background 0.98 0.98 0.98 1.0
  stroke 0 0 0 1 $ fill 1.0 0.0 0.0 0.2 $ translate (-0.2) (-0.2) $
    scale 0.5 $ part

testHSL2_render :: Int -> Int -> C.Render ()
testHSL2_render px py = do
  let rg = R.mkStdGen 12345
  C.save
  CB.preamble px py
  CB.renderCairo rg 1e-5 $ testHSL2
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

testSquare_render :: Int -> Int -> C.Render ()
testSquare_render px py = do
  let rg = R.mkStdGen 12345
  C.save
  CB.preamble px py
  CB.renderCairo rg 1e-5 testSquare
  C.restore

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
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ testHSL2_render px py
    C.surfaceWriteToPNG surf ("testHSL2.png")
  C.withSVGSurface "testHSL.svg"
    (fromIntegral px) (fromIntegral py) $
    \surf -> C.renderWith surf $ testHSL_render px py
  DT.writeFile "testHSL2_blaze.svg" $ BB.render (R.mkStdGen 12345) 1e-5 px py testHSL2

  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ testSquare_render px py
    C.surfaceWriteToPNG surf ("testSquare.png")
  DT.writeFile "testSquare_blaze.svg" $ BB.render (R.mkStdGen 12345) 1e-2 px py testSquare

  {-
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ sierpinski_render px py
    C.surfaceWriteToPNG surf ("sierpinski.png")
  DT.writeFile "sierpinski_blaze.svg" $ BB.render (R.mkStdGen 12345) 1e-2 px py sierpinski

  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ notSierpinski_render px py
    C.surfaceWriteToPNG surf ("notSierpinski.png")
  C.withSVGSurface "notSierpinski.svg"
    (fromIntegral px) (fromIntegral py) $
    \surf -> C.renderWith surf $ notSierpinski_render px py
  DT.writeFile "notSierpinski_blaze.svg" $ BB.render (R.mkStdGen 12345) 1e-2 px py notSierpinski
  -}
