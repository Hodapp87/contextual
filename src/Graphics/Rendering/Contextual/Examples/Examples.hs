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

import Graphics.Rendering.Contextual.Core
import Graphics.Rendering.Contextual.Colors
import qualified Graphics.Rendering.Contextual.Backend.Cairo as CB
import qualified Graphics.Rendering.Contextual.Backend.Blaze as BB

import qualified Graphics.Rendering.Cairo as C 

import qualified System.Random as R

testHSL :: Node ()
testHSL = do
  let r = do
        -- For each 'r', draw a square...
        square
        -- Then shift hue, decrease chroma, increase luminance, and
        -- make less transparent:
        shift Fill Hue (-0.3) $
          shift Fill Chrom (-6) $
          shift Fill Lum 5 $
          shift Fill Alpha 0.07 $
          -- Then scale down to 90%, move a bit to the right and
          -- slightly up, rotate pi/8 radians - and repeat 'r' again.
          rotate (pi/8) $
          translate 0.5 (-0.15) $
          scale 0.9 r
  -- Start with a fully black background:
  background (0, 0, 0, 1)
  -- Start with a fill color that's about 50% luminance, @b*=-100@
  -- (fully-saturated blue-ish), and 30% opacity:
  set Fill (50, 0, -100, 0.3) $
    -- Begin with @r@, scaled down to 35%, and shifted towards the
    -- top-left corner a bit.
    translate (-0.25) (-0.25) $
    scale 0.35 r

testHSL2 :: Node ()
testHSL2 = do
  let part = do
        square
        shift Fill Hue 0.1 $
          shift Fill Chrom 3 $
          shift Fill Lum (-2) $
          --shift Fill Alpha 0.07 $
          rotate (pi/7) $
          translate 0.4 (-0.05) $
          scale2 0.9 0.7 part
  background (98, 0, 0, 1)
  set Stroke (0, 0, 0, 1) $
    set Fill (70, 80, 40, 0.3) $
    translate (-0.2) (-0.2) $
    scale 0.5 part

-- | Probabilistic subdivision of a square
quadtree :: Node ()
quadtree = do
  let p = 0.75
      r' n = random (1-p) (return ()) n
      d = 0.5
      f = 3
      squareR = do
        square
        -- 'p' chance of recursing in each quadrant:
        r' $ shift' Fill [(Hue, 0.21), (Chrom, 1/f), (Lum, f)] $
          scale 0.5 $ translate d d $ squareR
        r' $ shift' Fill [(Hue, -0.19), (Chrom, f), (Lum, 1/f)] $
          scale 0.5 $ translate d (-d) $ squareR
        r' $ shift' Fill [(Hue, 0.09), (Chrom, 1/f), (Lum, 1/f)] $
          scale 0.5 $ translate (-d) d $ squareR
        r' $ shift' Fill [(Hue, -0.14), (Chrom, f), (Lum, f)] $
          scale 0.5 $ translate (-d) (-d) $ squareR
  background (0, 0, 0, 1)
  scale 0.95 $ set Stroke (0, 0, 0, 0.5) $ set Fill (90, 20, -50, 1.0) $ squareR

-- | Probabilistic subdivision of a square, with another step
quadtree45 :: Node ()
quadtree45 = do
  let p = 0.75
      r' n = random (1-p) (return ()) n
      d = 0.5
      f = 3
      p_ang = 0.3
      ang = pi/7
      s_ang = 1 / (cos ang + sin ang)
      squareR = do
        square
        -- 'p45' chance of a 45-degree rotation with corners touching
        -- sides:
        random p_ang
          (shift Fill Hue (-2) $ scale s_ang $ rotate ang squareR)
          $ do
          -- 'p' chance of recursing in each quadrant:
          r' $ shift' Fill [(Hue, 0.21), (Chrom, 1/f), (Lum, f)] $
            scale 0.5 $ translate d d $ squareR
          r' $ shift' Fill [(Hue, -0.19), (Chrom, f), (Lum, 1/f)] $
            scale 0.5 $ translate d (-d) $ squareR
          r' $ shift' Fill [(Hue, 0.09), (Chrom, 1/f), (Lum, 1/f)] $
            scale 0.5 $ translate (-d) d $ squareR
          r' $ shift' Fill [(Hue, -0.14), (Chrom, f), (Lum, f)] $
            scale 0.5 $ translate (-d) (-d) $ squareR
  background (0, 0, 0, 1)
  scale 0.95 $ set Stroke (0, 0, 0, 0.5) $ set Fill (90, 20, -50, 1.0) $ squareR

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

stroke = undefined
fill = undefined
shiftRGBA = undefined

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
  --background 0.0 0.0 0.0 1.0
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
  --background 0.0 0.0 0.0 1.0
  shiftRGBA 0.8 1.4 3.0 1.3 $ stroke 0 0 0 0 $ fill 1.0 0.3 0.3 0.2 $ rep6

-- | This is a render for the sake of checking bounds of backends; it
-- contains a square which fills the entire canvas, and another one in
-- front with a slight margin.
testSquare :: Node ()
testSquare = do
  -- Red square fills entire canvas:
  set Fill (100, 200, 200, 0.5) square
  -- White square, slightly smaller, sits in front:
  scale 0.95 $ set Fill (100, 0, 0, 0.5) square
  set Fill (100, 0, 0, 0.5) triangle

-- | Test out the line primitive
testLines :: Node ()
testLines = do
  let r = do
        line
        scale 0.5 $ translate 1 0 $ do
          rotate (pi/6) $ r
          rotate (-pi/6) $ r
  background (100, 0, 0, 1)
  set Stroke (0, 0, 0, 1.0) $ scale 0.5 $ do
    mapM_ (\ang -> rotate ang r) [0, pi/2, pi, 3*pi/2]

main :: IO ()
main = do
  let px = 4000
      py = 4000

  DT.writeFile "testHSL_blaze_LAB.svg" $
    BB.render (R.mkStdGen 12345) 1e-5 px py testHSL
    
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12345) 1e-5 px py testHSL
    C.surfaceWriteToPNG surf ("testHSL_LAB.png")

  DT.writeFile "testLines_blaze.svg" $
    BB.render (R.mkStdGen 12345) 1e-3 px py testLines
    
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12345) 1e-3 px py testLines
    C.surfaceWriteToPNG surf ("testLines_cairo.png")
  
  DT.writeFile "testHSL2_blaze_LAB.svg" $ BB.render (R.mkStdGen 12345) 1e-5 px py testHSL2
  DT.writeFile "testSquare_blaze_LAB.svg" $ BB.render (R.mkStdGen 12345) 1e-2 px py testSquare
  
  -- These two should look identical, except for in the tinier
  -- details.  That is, one should just be a 'more resolved' version
  -- of the other.
  DT.writeFile "quadtree_blaze_LAB.svg" $
    BB.render (R.mkStdGen 12347) 1e-2 px py quadtree
  DT.writeFile "quadtree_blaze_LAB_low.svg" $
    BB.render (R.mkStdGen 12347) 2e-2 px py quadtree
  -- This is the Cairo version, which may not match exactly thanks to
  -- differing colorspaces:
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12347) 1e-2 px py quadtree
    C.surfaceWriteToPNG surf ("quadtree_LAB.png")
  C.withImageSurface C.FormatARGB32 px py $ \surf -> do
    C.renderWith surf $ CB.renderCairo (R.mkStdGen 12347) 2e-2 px py quadtree
    C.surfaceWriteToPNG surf ("quadtree_LAB_low.png")
  DT.writeFile "quadtree45_blaze_LAB.svg" $
    BB.render (R.mkStdGen 12347) 1e-2 px py quadtree45
