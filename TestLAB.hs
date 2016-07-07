{-|
Module      : TestLAB
Description : Test renders for Contextual
Copyright   : (c) Chris Hodapp, 2016
License     : ?
Maintainer  : Hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

So far, this is a lot of scratch code for test renders that I have
done for Contextual.

This is a scratch file for testing LAB support.

-}

module Main where

import Control.Monad (forM_)

import qualified Data.Text.Lazy.IO as DT

import Contextual
-- import qualified CairoBackend as CB
import qualified BlazeBackend as BB
import qualified Graphics.Rendering.Cairo as C 

import qualified System.Random as R

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

-- | This is a render for the sake of checking bounds of backends; it
-- contains a square which fills the entire canvas, and another one in
-- front with a slight margin.
testSquare :: Node ()
testSquare = do
  -- Red square fills entire canvas:
  fill 1.0 0.0 0.0 0.5 square
  -- White square, slightly smaller, sits in front:
  scale 0.95 $ set Fill (100, 0, 0, 0.5) square
  set Fill (100, 0, 0, 0.5) triangle

main :: IO ()
main = do
  let px = 4000
      py = 4000

  DT.writeFile "testHSL_blaze_LAB.svg" $ BB.render (R.mkStdGen 12345) 1e-5 px py testHSL
  DT.writeFile "testHSL2_blaze_LAB.svg" $ BB.render (R.mkStdGen 12345) 1e-5 px py testHSL2
  DT.writeFile "testSquare_blaze_LAB.svg" $ BB.render (R.mkStdGen 12345) 1e-2 px py testSquare
