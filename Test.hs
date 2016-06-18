module Main where

import qualified ContextualToBlaze
import qualified ContextualExamples
import qualified Graphics.Rendering.Cairo as C 

main = putStrLn $ ContextualToBlaze.render ContextualExamples.spiral2

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
