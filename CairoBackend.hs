module CairoBackend where

import ContextualScraps

import Control.Monad.Free
import qualified Graphics.Rendering.Cairo as C 

renderCairo :: Show a => Ctxt a -> C.Render ()
renderCairo (Free (Scale n c' c)) = do
  C.save
  C.scale n n
  renderCairo c'
  C.restore
  renderCairo c
renderCairo (Free (Translate dx dy c' c)) = do
  C.save
  C.translate dx dy
  renderCairo c'
  C.restore
  renderCairo c
renderCairo (Free (Square c)) = do
  C.rectangle (-0.5) (-0.5) 1 1
  C.fill
  renderCairo c
renderCairo (Free t@_) = error $ "Unsupported type, " ++ show t
renderCairo (Pure _) = return ()

test :: Ctxt ()
test = do
  scale 0.5 $ square
  --translate 10.0 10.0 $ scale 20 $ do
  --  square
  --scale 2 $ translate 1.0 2.0 $ square

px = 400
py = 400

main = C.withImageSurface  
  C.FormatARGB32 px py $ \surf -> 
  do 
    C.renderWith surf $ do
      let px' = fromIntegral px
          py' = fromIntegral py
      C.save 
      C.setOperator C.OperatorOver
      --C.setSourceRGB 0 0 0 
      --C.rectangle 0 0 px' py'
      C.fill
      C.translate (px' / 2) (py' / 2)
      C.scale px' py'
      C.setSourceRGB 1 1 1
      renderCairo test
      C.restore 
    C.surfaceWriteToPNG surf "Text.png"
