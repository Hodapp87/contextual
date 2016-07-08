{-|
Module      : Cairo
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

module Graphics.Rendering.Contextual.Backend.Cairo
  (renderCairo, preamble)
where

import Graphics.Rendering.Contextual.Core hiding (scale)
import Graphics.Rendering.Contextual.Colors

import Control.Monad (when)
import Control.Monad.Free
import Control.Monad.State
import qualified System.Random as R

import qualified Data.Colour.CIE as CIE
import qualified Data.Colour.CIE.Illuminant
import qualified Data.Colour.SRGB as SRGB
import qualified Graphics.Rendering.Cairo as C 
import qualified Graphics.Rendering.Cairo.Matrix as CM

-- | Convert a LAB color to sRGB (given a whitepoint), and call
-- 'C.setSourceRGBA' on it, passing alpha through directly.
setSourceLAB :: CIE.Chromaticity Double -- ^ Whitepoint
             -> LABColor Double -- ^ Color
             -> C.Render ()
setSourceLAB wp (l,a,b,alpha) = let rgb = SRGB.toSRGB $ CIE.cieLAB wp l a b
  in C.setSourceRGBA (SRGB.channelRed rgb) (SRGB.channelGreen rgb)
     (SRGB.channelBlue rgb) alpha

renderCairo' :: (R.RandomGen a, Show b) => CIE.Chromaticity Double -- ^ Whitepoint
             -> Double -- ^ Minimum scale
             -> Node b -- ^ Starting node
             -> StateT (Context a) C.Render ()
renderCairo' wp minScale node = do
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
        renderCairo' wp minScale sub
        -- Restore the Cairo context:
        lift $ C.restore
        -- Restore our context, except for the RNG:
        modify $ \c -> ctxt { ctxtRand = ctxtRand c }
      -- Likewise, but with no transformation:
      xformAndRestore_ sub = xformAndRestore sub $ return ()
  case node of
    -- N.B. Only proceed if global scale is large enough
    (Free (Scale sx sy c n)) -> when (ctxtScale ctxt > minScale) $ do
      put $ ctxt { ctxtScale = ctxtScale ctxt * min sx sy }
      xformAndRestore c $ C.scale sx sy
      renderCairo' wp minScale n
    (Free (Translate dx dy c n)) -> do
      xformAndRestore c $ C.translate dx dy
      renderCairo' wp minScale n
    (Free (Rotate a c n)) -> do
      xformAndRestore c $ C.rotate a
      renderCairo' wp minScale n
    (Free (Shear sx sy c n)) -> do
      xformAndRestore c $ C.transform $ CM.Matrix 1.0 sx sy 1.0 0.0 0.0
      renderCairo' wp minScale n
    (Free (Square n)) -> do
      lift $ do
        C.rectangle (-0.5) (-0.5) 1 1
        setSourceLAB wp $ ctxtFill ctxt
        C.fill
        -- TODO: All of the below is unnecessary if no stroke is
        -- desired.  Right now, the only way to specify that no stroke
        -- should occur is to give a stroke with alpha=0, and then we
        -- still instruct Cairo to draw it anyway below.  A way to
        -- just disable stroke might be nice.
        C.rectangle (-0.5) (-0.5) 1 1
        setSourceLAB wp $ ctxtStroke ctxt
        C.stroke
      renderCairo' wp minScale n
    (Free (Triangle n)) -> do
      -- C.setLineWidth 5
      let h = 1.0 / sqrt(3.0)
          x n = h * cos (2 * n * pi / 3)
          y n = h * sin (2 * n * pi / 3)
      lift $ do
        C.moveTo (x 0) (y 0)
        C.lineTo (x 1) (y 1)
        C.lineTo (x 2) (y 2)
        C.closePath
        setSourceLAB wp $ ctxtFill ctxt
        C.fill
        setSourceLAB wp $ ctxtStroke ctxt
        C.stroke
      renderCairo' wp minScale n
    -- Note that we do not actually tell Cairo about colors until we
    -- actually need to draw something.  We *can* use C.setSourceRGBA
    -- anytime color changes in our own context, but there's no point.
    (Free (Set role color c n)) -> do
      -- Render child nodes in a modified context:
      put $ case role of Stroke -> ctxt { ctxtStroke = color }
                         Fill   -> ctxt { ctxtFill = color }
      xformAndRestore_ c
      renderCairo' wp minScale n
    (Free (Shift role view f c n)) -> do
      -- Get existing color and transform it:
      let color = case role of Stroke -> ctxtStroke ctxt
                               Fill -> ctxtFill ctxt
          color' = shiftColor view f color
      -- Render child nodes in a modified context:
      put $ case role of Stroke -> ctxt { ctxtStroke = color' }
                         Fill   -> ctxt { ctxtFill = color' }
      xformAndRestore_ c
      renderCairo' wp minScale n
      -- TODO: Most of above is identical with the 'Set' case; put it
      -- in one place
    (Free (Random p c1 c2 n)) -> do
      -- Get a random sample in [0,1]:
      let g = ctxtRand ctxt
          (sample, g') = R.random g
      put $ ctxt { ctxtRand = g' }
      renderCairo' wp minScale $ if sample < p then c1 else c2
      renderCairo' wp minScale n
    (Free (Background color n)) -> do
      -- save & restore is probably overkill here, but this should
      -- only be done once.
      lift $ do
        C.save
        setSourceLAB wp color
        C.paint
        C.restore
      renderCairo' wp minScale n
    (Free t) -> error $ "Unsupported type in renderCairo, " ++ show t
    (Pure _) -> return ()

-- The above method probably makes more excessive use of save/restore
-- than is strictly necessary.  We could compose transformations on
-- our own and 'flatten' things to not require a stack.

-- | Produce a Cairo render from a 'Node' and some starting information.
renderCairo :: (Show a, R.RandomGen r) => r -- ^ Random generator
            -> Double -- ^ Minimum scale (below this, recursion
                      -- terminates)
            -> Int -- ^ Width in pixels
            -> Int -- ^ Height in pixels
            -> Node a -- ^ Scene to render
            -> C.Render ()
renderCairo rg minScale px py node = do
  let wp = Data.Colour.CIE.Illuminant.d65
  preamble px py
  execStateT (renderCairo' wp minScale node) $ defaultContext rg
  return ()
  -- TODO: Fix the types around here as they need not all be ()

-- | Sets up a new environment for a given image size in pixels.  This
-- sets up the coordinate space to go from (-1/2,-1/2) to (1/2,1/2).
-- Calling 'C.save' beforehand or 'C.restore' after is still up to
-- you.
preamble :: Int -> Int -> C.Render ()
preamble px py = do
  let px' = fromIntegral px
      py' = fromIntegral py
  C.setOperator C.OperatorOver
  C.translate (px' / 2) (py' / 2)
  C.scale px' py'
  -- TODO: Make line width configurable!
  C.setLineWidth 0.005
