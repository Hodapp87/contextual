{-|
Module      : CairoBackend
Description : Cairo backend for Contextual
Copyright   : (c) Chris Hodapp, 2016
License     : ?
Maintainer  : Hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

This is the backend for rendering a 'Node' grammar to Cairo,
particularly, the type
<https://hackage.haskell.org/package/cairo-0.12.3/docs/Graphics-Rendering-Cairo.html
Graphics.Rendering.Cairo.Render>.

-}

{-# LANGUAGE FlexibleContexts #-}

module CairoBackend (renderCairo, preamble, ColorRGBA(..)) where

import Contextual

import Control.Monad (when)
import Control.Monad.Free
import Control.Monad.State
import qualified System.Random as R

import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.RGBSpace.HSL as HSL
import qualified Graphics.Rendering.Cairo as C 
import qualified Graphics.Rendering.Cairo.Matrix as CM

-- | Wrapper around 'C.setSourceRGBA' for 'ColorRGBA' arguments
setSourceRGBA' :: ColorRGBA -> C.Render ()
setSourceRGBA' c = C.setSourceRGBA (colorR c) (colorG c) (colorB c) (colorA c)

renderCairo' :: R.RandomGen a => Double -- ^ Minimum scale
             -> Node b -- ^ Starting node
             -> StateT (Context a) C.Render ()
renderCairo' minScale node = do
  ctxt <- get
  let -- Add some transformation, render the sub-node, and remove that
      -- transformation:
      --
      --xformAndRestore :: C.Render () -> Node b -> StateT (Context a) C.Render ()
      xformAndRestore sub xform = do
        -- Start Cairo context, apply transformation:
        lift $ do C.save
                  xform
        -- Recurse (assuming our own context is correct):
        renderCairo' minScale sub
        -- Restore the Cairo context:
        lift $ C.restore
        -- Restore our context, but pass forward the RNG:
        g <- ctxtRand <$> get
        put $ ctxt { ctxtRand = g }
      -- Likewise, but with no transformation:
      xformAndRestore_ sub = xformAndRestore sub $ return ()
  case node of
    -- N.B. Only proceed if global scale is large enough
    (Free (Scale sx sy c' c)) -> when (ctxtScale ctxt > minScale) $ do
      put $ ctxt { ctxtScale = ctxtScale ctxt * min sx sy }
      xformAndRestore c' $ C.scale sx sy
      renderCairo' minScale c
    (Free (Translate dx dy c' c)) -> do
      xformAndRestore c' $ C.translate dx dy
      renderCairo' minScale c
    (Free (Rotate a c' c)) -> do
      xformAndRestore c' $ C.rotate a
      renderCairo' minScale c
    (Free (Shear sx sy c' c)) -> do
      xformAndRestore c' $ C.transform $ CM.Matrix 1.0 sx sy 1.0 0.0 0.0
      renderCairo' minScale c
    (Free (Square c)) -> do
      lift $ do
        C.rectangle (-0.5) (-0.5) 1 1
        setSourceRGBA' $ ctxtFill ctxt
        C.fill
        C.rectangle (-0.5) (-0.5) 1 1
        setSourceRGBA' $ ctxtStroke ctxt
        C.stroke
      renderCairo' minScale c
    (Free (Triangle c)) -> do
      -- C.setLineWidth 5
      let c60 = cos (pi/3) / 2
          s60 = sin (pi/3) / 2
      lift $ do
        C.moveTo 0.5 0.0
        C.lineTo (-c60) s60
        C.lineTo (-c60) (-s60)
        C.closePath
        setSourceRGBA' $ ctxtFill ctxt
        C.fill
        setSourceRGBA' $ ctxtStroke ctxt
        C.stroke
      renderCairo' minScale c
    -- Note that we do not actually tell Cairo about colors until we
    -- actually need to draw something.  We *can* use C.setSourceRGBA
    -- anytime color changes in our own context, but there's no point.
    (Free (Fill r g b a c' c)) -> do
      let rgba = ctxtFill ctxt
          rgba' = clampRGBA $ ColorRGBA { colorR = r
                                        , colorG = g
                                        , colorB = b
                                        , colorA = a
                                        }
      -- TODO: This is nearly identical to ShiftRGBA.  I should
      -- probably factor it out somehow.
      put $ ctxt { ctxtFill = rgba' }
      xformAndRestore_ c'
      renderCairo' minScale c
    (Free (Stroke r g b a c' c)) -> do
      let rgba = ctxtStroke ctxt
          rgba' = clampRGBA $ ColorRGBA { colorR = r
                                        , colorG = g
                                        , colorB = b
                                        , colorA = a
                                        }
      put $ ctxt { ctxtStroke = rgba' }
      xformAndRestore_ c'
      renderCairo' minScale c
    (Free (Random p c1 c2 c)) -> do
      -- Get a random sample in [0,1]:
      let g = ctxtRand ctxt
          (sample, g') = R.random g
      put $ ctxt { ctxtRand = g' }
      renderCairo' minScale (if sample < p then c1 else c2)
      renderCairo' minScale c
    (Free (ShiftRGBA r g b a c' c)) -> do
      let rgba = ctxtFill ctxt
          rgba' = clampRGBA $ ColorRGBA { colorR = colorR rgba * r
                                        , colorG = colorG rgba * g
                                        , colorB = colorB rgba * b
                                        , colorA = colorA rgba * a
                                        }
      put $ ctxt { ctxtFill = rgba' }
      xformAndRestore_ c'
      renderCairo' minScale c
    (Free (ShiftHSL dh sf lf af c' c)) -> do
      let rgba = ctxtFill ctxt
          -- Get HSL & transform:
          (h,s,l) = HSL.hslView $
            SRGB.RGB (colorR rgba) (colorG rgba) (colorB rgba)
          (h', s', l') = (h + dh, clamp $ s * sf, clamp $ l * lf)
          -- Get RGB, and transform alpha separately:
          rgb' = HSL.hsl h' s' l'
          rgba' = ColorRGBA { colorR = SRGB.channelRed   rgb'
                            , colorG = SRGB.channelGreen rgb'
                            , colorB = SRGB.channelBlue  rgb'
                            , colorA = clamp $ colorA rgba * af
                            }
      put $ ctxt { ctxtFill = rgba' }
      xformAndRestore_ c'
      renderCairo' minScale c
    (Free (Background r g b a c)) -> do
      -- save & restore is probably overkill here, but this should
      -- only be done once.
      lift $ do
        C.save
        C.setSourceRGBA r g b a
        C.paint
        C.restore
      renderCairo' minScale c
    (Free _) -> error $ "Unsupported type in renderCairo"
    (Pure _) -> return ()

-- The above method probably makes more excessive use of save/restore
-- than is strictly necessary.  We could compose transformations on
-- our own and 'flatten' things to not require a stack.

-- | Produce a Cairo render from a 'Node' and some starting information.
renderCairo :: R.RandomGen r => r -- ^ Random generator
            -> Double -- ^ Minimum scale (below this, recursion
                      -- terminates)
            -> Node a -- ^ Scene to render
            -> C.Render ()
renderCairo rg minScale node = do
  let startCtxt = defaultContext rg
  execStateT (renderCairo' minScale node) startCtxt
  return ()
  -- TODO: Fix the types around here as they need not all be ()

-- | Sets up a new environment for a given image size in pixels.  This
-- sets up the coordinate space to go from (-1,-1) to (1,1).  Calling
-- 'C.save' beforehand or 'C.restore' after is still up to you.
preamble :: Int -> Int -> C.Render ()
preamble px py = do
  let px' = fromIntegral px
      py' = fromIntegral py
  C.setOperator C.OperatorOver
  C.translate (px' / 2) (py' / 2)
  C.scale px' py'
  -- TODO: Make line width configurable!
  C.setLineWidth 0.005
