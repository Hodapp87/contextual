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

test :: Ctxt ()
test = do
  let d = 0.7
      angle = 0.4
  scale 0.75 $ do
    pattern   d    0  angle
    pattern (-d)   0  angle
    pattern   0    d  angle
    pattern   0  (-d) angle

pattern :: Double -> Double -> Double -> Ctxt ()
pattern dx dy angle = do
  scale 0.5 $ do
    square
    translate dx dy $ rotate angle $ pattern dx dy angle

px = 600
py = 600

main = C.withImageSurface  
  C.FormatARGB32 px py $ \surf -> 
  do 
    C.renderWith surf $ do
      -- Open a context (not strictly needed, I don't think, but it
      -- may help if we ever need to compose anything):
      C.save
      preamble px py
      --C.setSourceRGB 0 0 0 
      --C.rectangle 0 0 px' py'
      --C.fill
      renderCairo 1e-6 test
      C.restore 
    C.surfaceWriteToPNG surf "test.png"
