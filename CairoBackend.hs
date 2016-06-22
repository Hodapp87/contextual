module CairoBackend where

import Contextual

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

-- | Sets up a new environment for a given image size in pixels.  This
-- sets up the coordinate space to go from (0,0) to (1,1).  Calling
-- 'C.save' beforehand or 'C.restore' after is still up to you.
preamble :: Int -> Int -> C.Render ()
preamble px py = do
  let px' = fromIntegral px
      py' = fromIntegral py
  C.setOperator C.OperatorOver      
  C.translate (px' / 2) (py' / 2)
  C.scale px' py'
  C.setSourceRGB 1 1 1
