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
  pattern 0.5 0
  pattern (-0.5) 0
  pattern 0 0.5
  pattern 0 (-0.5)

pattern :: Double -> Double -> Ctxt ()
pattern dx dy = do
  scale 0.5 $ do
    square
    translate dx dy $ pattern dx dy

px = 400
py = 400

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
      renderCairo 1e-5 test
      C.restore 
    C.surfaceWriteToPNG surf "Text.png"
