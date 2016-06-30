module Main where

import Contextual
import CairoBackend
import qualified Graphics.Rendering.Cairo as C 

{-
main2 = C.withImageSurface  
  C.FormatARGB32 200 200 $ \surf -> 
  do 
    C.renderWith surf $ do
      C.save 
      C.setOperator C.OperatorOver
      C.setSourceRGB 0 0 0 
      C.rectangle 0 0 100 100 
      C.fill      
      C.setSourceRGB 1 1 1 
      --C.selectFontFace "Trebuchet MS"  
      --  C.FontSlantNormal
      --  C.FontWeightNormal 
      C.setFontSize 18 
      C.textPath "Hello world"
      C.fill
      C.setSourceRGB 1 1 0
      C.setLineWidth 5
      C.moveTo 120 60
      C.lineTo 60 110
      C.lineTo 180 110
      C.closePath
      C.stroke
      C.restore 
    C.surfaceWriteToPNG surf "Text.png"
-}

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
  colorshift 0.8 1.4 3.0 1.3 $ rep6

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

px = 4000
py = 4000

main = C.withImageSurface  
  C.FormatARGB32 px py $ \surf -> 
  do 
    C.renderWith surf $ do
      -- Open a context (not strictly needed, I don't think, but it
      -- may help if we ever need to compose anything):
      C.save
      preamble px py True
      renderCairo 0.5e-3 (ColorRGBA 1.0 0.3 0.3 0.2) sierpinski
      C.restore 
    C.surfaceWriteToPNG surf "sierpinski.png"
