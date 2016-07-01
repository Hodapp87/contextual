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

import Contextual hiding (scale)

import Control.Monad (when)
import Control.Monad.Free
import Control.Monad.State
import qualified System.Random as R

import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.RGBSpace.HSL as HSL
import qualified Graphics.Rendering.Cairo as C 
import qualified Graphics.Rendering.Cairo.Matrix as CM

data ColorRGBA = ColorRGBA { colorR :: Double
                           , colorG :: Double
                           , colorB :: Double
                           , colorA :: Double
                           } deriving Show

-- | Wrapper around 'C.setSourceRGBA' for 'ColorRGBA' arguments
setSourceRGBA' :: ColorRGBA -> C.Render ()
setSourceRGBA' c = C.setSourceRGBA (colorR c) (colorG c) (colorB c) (colorA c)

clamp :: (Ord f, Num f) => f -> f
clamp v = if v < 0 then 0 else if v > 1 then 1 else v

-- | Clamp a 'ColorRGBA' so all colors are within [0,1].
clampRGBA :: ColorRGBA -> ColorRGBA
clampRGBA rgba = ColorRGBA { colorR = clamp $ colorR rgba
                           , colorG = clamp $ colorG rgba
                           , colorB = clamp $ colorB rgba
                           , colorA = clamp $ colorA rgba
                           }

-- | Rendering context
data Context a = Context { scale :: Double -- ^ Overall scale
                         , fill :: ColorRGBA -- ^ Current fill color
                         , rand :: a -- ^ RandomGen
                         }

renderCairo' :: R.RandomGen a => Double -- ^ Minimum scale
             -> Node b -- ^ Starting node
             -> StateT (Context a) C.Render ()
renderCairo' minScale node = do
  ctxt <- get
  let --xformAndRestore :: C.Render () -> Node b -> StateT (Context a) C.Render ()
      xformAndRestore sub xform = do
        -- Start Cairo context, apply transformation:
        lift $ do C.save
                  xform
        -- Recurse (assuming our own context is correct):
        renderCairo' minScale sub
        -- Restore the Cairo context:
        lift $ C.restore
        -- Restore our context, but pass forward the RNG:
        g <- rand <$> get
        put $ ctxt { rand = g }
  case node of
    -- N.B. Only proceed if global scale is large enough
    (Free (Scale n c' c)) -> when (scale ctxt > minScale) $ do
      put $ ctxt { scale = scale ctxt * n }
      xformAndRestore c' $ C.scale n n
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
      lift $ C.rectangle (-0.5) (-0.5) 1 1
      lift $ C.fill
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
        C.fill
      renderCairo' minScale c
    (Free (ShiftRGBA r g b a c' c)) -> do
      let rgba = fill ctxt
          rgba' = clampRGBA $ ColorRGBA { colorR = colorR rgba * r
                                        , colorG = colorG rgba * g
                                        , colorB = colorB rgba * b
                                        , colorA = colorA rgba * a
                                        }
      put $ ctxt { fill = rgba' }
      xformAndRestore c' $ setSourceRGBA' rgba'
      renderCairo' minScale c
    (Free (ShiftHSL dh sf lf af c' c)) -> do
      let rgba = fill ctxt
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
      put $ ctxt { fill = rgba' }
      xformAndRestore c' $ setSourceRGBA' rgba'
      renderCairo' minScale c
    (Free (Random p c1 c2 c)) -> do
      -- Get a random sample in [0,1]:
      let g = rand ctxt
          (sample, g') = R.random g
      put $ ctxt { rand = g' }
      renderCairo' minScale (if sample < p then c1 else c2)
      renderCairo' minScale c
    (Free _) -> error $ "Unsupported type in renderCairo"
    (Pure _) -> return ()

renderCairo :: R.RandomGen r => r -> Double -> ColorRGBA -> Node a -> C.Render ()
renderCairo rg minScale color node = do
  let startCtxt = Context { scale = 1.0, fill = color, rand = rg }
  setSourceRGBA' color
  execStateT (renderCairo' minScale node) startCtxt
  return ()
  -- TODO: Fix the types around here as they need not all be ()

-- The above method probably makes more excessive use of save/restore
-- than is strictly necessary.  We could compose transformations on
-- our own and 'flatten' things to not require a stack.

-- | Sets up a new environment for a given image size in pixels.  This
-- sets up the coordinate space to go from (0,0) to (1,1).  If 'black'
-- is True, then the background is filled with black; otherwise, it is
-- left transparent. Calling 'C.save' beforehand or 'C.restore' after
-- is still up to you.
preamble :: Int -> Int -> Bool -> C.Render ()
preamble px py black = do
  let px' = fromIntegral px
      py' = fromIntegral py
  C.setOperator C.OperatorOver
  when black $ do
    C.setSourceRGB 0 0 0 
    C.rectangle 0 0 px' py'
    C.fill
  C.translate (px' / 2) (py' / 2)
  C.scale px' py'
  C.setSourceRGBA 1 1 1 0.3
