module Main where

import Contextual
import CairoBackend
import qualified Graphics.Rendering.Cairo as C 

import qualified System.Random as R

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

testRandom :: Node ()
testRandom = do
  square
  colorshift 0.8 1.4 2.0 1.0 $ do
    random 0.25
      (translate (1/2)  0 $ scale 0.75 $ rotate (pi/3) testRandom)
      (translate 0 (-1/2) $ scale 0.75 $ rotate (-pi/3) testRandom)
    random 0.1
      (translate (-1/2) 0 $ scale 0.5 $ testRandom)
      (return ())

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

main = do
  let sierpR :: C.Render ()
      sierpR = do
        -- TODO: Fix below (don't actually need random generator for
        -- deterministic grammars):
        let rg = R.mkStdGen 12345
        -- Open a context (not strictly needed, I don't think, but it
        -- may help if we ever need to compose anything):
        C.save
        preamble px py True
        renderCairo rg 0.5e-3 (ColorRGBA 1.0 0.3 0.3 0.2) sierpinski
        --renderCairo 0.5e-3 (ColorRGBA 1.0 0.3 0.3 0.2) $ scale 0.5 $ testRandom
        C.restore
      randR :: Int -> C.Render ()
      randR seed = do
        let rg = R.mkStdGen seed
        C.save
        preamble px py True
        renderCairo rg 1e-5 (ColorRGBA 1.0 0.3 0.3 0.2) $ scale 0.5 $ testRandom
        C.restore
  C.withImageSurface  
    C.FormatARGB32 px py $ \surf -> 
    do
      let seed = 12347
      C.renderWith surf $ randR seed
      C.surfaceWriteToPNG surf ("testRandom" ++ show seed ++ ".png")
  {-
  C.withSVGSurface "sierpinski.svg"
    (fromIntegral px) (fromIntegral py) $ \surf ->
    do
      C.renderWith surf rendered-}
