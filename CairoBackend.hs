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

module CairoBackend where

import Contextual

--import Debug.Trace

import Control.Monad (when)
import Control.Monad.Free
import Control.Monad.State
import qualified Graphics.Rendering.Cairo as C 
import qualified Graphics.Rendering.Cairo.Matrix as CM

-- For my reference:
-- https://hackage.haskell.org/package/cairo-0.12.3/docs/Graphics-Rendering-Cairo.html

data ColorRGBA = ColorRGBA { colorR :: Double
                           , colorG :: Double
                           , colorB :: Double
                           , colorA :: Double
                           }

-- | Wrapper around 'C.setSourceRGBA' for 'ColorRGBA' arguments
setSourceRGBA' :: ColorRGBA -> C.Render ()
setSourceRGBA' c = C.setSourceRGBA (colorR c) (colorG c) (colorB c) (colorA c)



renderCairo' :: StateT [Int] C.Render ()
renderCairo' = undefined

-- | Render a 'Node' to Cairo, given some minimum scale at which
-- rendering stops and a starting color.
renderCairo :: Show a => Double -> ColorRGBA -> Node a -> C.Render ()
renderCairo minScale color ctxt = setSourceRGBA' color >>
                                  renderRec 1.0 color ctxt
  where renderRec :: Show a => Double -> ColorRGBA -> Node a -> C.Render ()
        -- Below are all transformations.  Note that we always use
        -- 'C.save' beforehand, and 'C.restore' after.
        renderRec gs rgba (Free (Scale n c' c)) = do
          -- This is a little rudimentary, but it should work.  Only
          -- proceed into nested items when global scale is larger
          -- than 'minScale':
          when (gs > minScale) $ do
            C.save
            C.scale n n
            renderRec (n * gs) rgba c'
            C.restore
          renderRec gs rgba c
        renderRec gs rgba (Free (Translate dx dy c' c)) = do
          -- The pattern below is the same for every transformation so
          -- far, and could probably be factored out:
          C.save
          C.translate dx dy
          renderRec gs rgba c'
          C.restore
          renderRec gs rgba c
        renderRec gs rgba (Free (Rotate a c' c)) = do
          C.save
          C.rotate a
          renderRec gs rgba c'
          C.restore
          renderRec gs rgba c
        renderRec gs rgba (Free (Shear sx sy c' c)) = do
          C.save
          C.transform $ CM.Matrix 1.0 sx sy 1.0 0.0 0.0
          renderRec gs rgba c'
          C.restore
          renderRec gs rgba c
        -- Below are primitives:
        renderRec gs rgba (Free (Square c)) = do
          C.rectangle (-0.5) (-0.5) 1 1
          C.fill
          renderRec gs rgba c
        renderRec gs rgba (Free (Triangle c)) = do
          -- C.setLineWidth 5
          let c60 = cos (pi/3) / 2
              s60 = sin (pi/3) / 2
          C.moveTo 0.5 0.0
          C.lineTo (-c60) s60
          C.lineTo (-c60) (-s60)
          C.closePath
          C.fill
          renderRec gs rgba c
        renderRec gs rgba (Free (ColorShift r g b a c' c)) = do
          C.save
          let rgba' = ColorRGBA { colorR = colorR rgba * r
                                , colorG = colorG rgba * g
                                , colorB = colorB rgba * b
                                , colorA = colorA rgba * a
                                }
          setSourceRGBA' rgba'
          renderRec gs rgba' c'
          C.restore
          renderRec gs rgba c
        renderRec _ _ (Free t@_) = error $ "Unsupported type, " ++ show t
        renderRec _ _ (Pure _) = return ()

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
