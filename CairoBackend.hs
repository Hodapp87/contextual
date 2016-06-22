module CairoBackend where

import Contextual

--import Debug.Trace

import Control.Monad (when)
import Control.Monad.Free
import qualified Graphics.Rendering.Cairo as C 

-- | Render a 'Ctxt' to Cairo, given some minimum scale at which
-- rendering stops.
renderCairo :: Show a => Double -> Ctxt a -> C.Render ()
renderCairo minScale = renderRec 1.0
  where renderRec :: Show a => Double -> Ctxt a -> C.Render ()
        renderRec gs (Free (Scale n c' c)) = do
          -- This is a little rudimentary, but it should work.  Only
          -- proceed into nested items when global scale is larger
          -- than 'minScale':
          when (gs > minScale) $ do
            C.save
            C.scale n n
            renderRec (n * gs) c'
            C.restore
          renderRec gs c
        renderRec gs (Free (Translate dx dy c' c)) = do
          C.save
          C.translate dx dy
          renderRec gs c'
          C.restore
          renderRec gs c
        renderRec gs (Free (Square c)) = do
          C.rectangle (-0.5) (-0.5) 1 1
          C.fill
          renderRec gs c
        renderRec _ (Free t@_) = error $ "Unsupported type, " ++ show t
        renderRec _ (Pure _) = return ()

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
