{-|
Module      : BlazeBackend
Description : Blaze (particularly blaze-svg) backend for Contextual
Copyright   : (c) Chris Hodapp, 2016
License     : ?
Maintainer  : Hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

This is the backend for rendering a 'Node' grammar to an SVG via
<https://hackage.haskell.org/package/blaze-svg blaze-svg>.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BlazeBackend where

import Contextual hiding (scale)

import Control.Monad (when)
import Control.Monad.Free
import Control.Monad.State
--import Data.Text
import Data.List (intercalate)
import qualified System.Random as R

import Data.Colour
import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.RGBSpace.HSL as HSL

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

render :: R.RandomGen r => r -- ^ Random generator
       -> Double -- ^ Minimum scale (below this, recursion terminates)
       -> ColorRGBA -- ^ Starting color
       -> Node a -- ^ Scene to render
       -> S.Svg
render rg minScale color node = do
  let --(x0,y0) = topLeft scene
      --(x1,y1) = bottomRight scene
      (x0,x1) = (0,1)
      (y0,y1) = (0,1)
      w = 400 * (x1 - x0)
      h = 400 * (y1 - y0)
      startCtxt = Context { ctxtScale = 1.0, ctxtFill = color, ctxtRand = rg }
  -- Preamble
  S.docTypeSvg ! A.version "1.1" !
  -- Attributes for width & height:
    (A.width $ S.toValue $ show w) !
    (A.height $ S.toValue $ show h) !
    -- Attribute for viewbox, e.g. viewbox "0 0 1 1":
    (A.viewbox $ S.toValue $ intercalate " " $ map show [x0,y0,x1,y1]) $
    do
      -- Background:
      -- S.rect ! (A.fill $ S.toValue $ SRGB.sRGB24show _)
      -- Actual scene:
      evalState (render' minScale node) startCtxt

tempAttribs :: S.Attribute
tempAttribs = mconcat [ --A.fill "#8080FF"
                      --, A.stroke "#404000"
                      --, A.strokeOpacity "0.50"
                      A.strokeWidth "0.005"
                      --, A.fillOpacity "0.15"
                      ]

-- Problem: I want to lift operations into S.Svg.  I can't really
-- lift, except into a StateT.  S.Svg is MarkupM (), so StateT
-- (Context a) S.Svg doesn't work (kind is wrong).  MarkupM is
-- internal to blaze, however.  I can use it, but it seems like I
-- shouldn't have to.

render' :: R.RandomGen a => Double -- ^ Minimum scale
        -> Node b -- ^ Starting node
        -> State (Context a) S.Svg
render' minScale node = do
  ctxt <- get
  {-let --xformAndRestore :: C.Render () -> Node b -> StateT (Context a) C.Render ()
      xformAndRestore sub xform = do
        -- Start Cairo context, apply transformation:
        lift $ do C.save
                  xform
        -- Recurse (assuming our own context is correct):
        render' minScale sub
        -- Restore the Cairo context:
        lift $ C.restore
        -- Restore our context, but pass forward the RNG:
        g <- ctxtRand <$> get
        put $ ctxt { ctxtRand = g }-}
  case node of
    -- N.B. Only proceed if global scale is large enough
    (Free (Scale sx sy c' c)) -> {- when (ctxtScale ctxt > minScale) $ -} do
      --put $ ctxt { ctxtScale = ctxtScale ctxt * min sx sy }
      --xformAndRestore c' $ C.scale sx sy
      render' minScale c
    (Free (Translate dx dy c' c)) -> do
      --xformAndRestore c' $ C.translate dx dy
      render' minScale c
    (Free (Rotate a c' c)) -> do
      --xformAndRestore c' $ C.rotate a
      render' minScale c
    (Free (Shear sx sy c' c)) -> do
      --xformAndRestore c' $ C.transform $ CM.Matrix 1.0 sx sy 1.0 0.0 0.0
      render' minScale c
    (Free (Square c)) -> do
      -- I don't think this is right either...
      return $ do
        (S.rect ! A.width "1" ! A.height "1" !
          (A.transform $ S.translate (-0.5) (-0.5)) ! tempAttribs)
        -- N.B. Translate (-0.5,-0.5) so it is centered at (0,0).
      render' minScale c
    (Free (Triangle c)) -> do
      let h = 1.0 / sqrt(3.0)
          ang n = 2 * n * pi / 3
          pt = \ang -> show (h * cos(ang)) ++ "," ++ show (h * sin(ang)) ++ " "
          ptStr = pt (ang 0) ++ pt (ang 1) ++ pt (ang 2)      
      return $ S.polygon ! (A.points $ S.toValue ptStr) ! tempAttribs
      render' minScale c
    (Free (Random p c1 c2 c)) -> do
      -- Get a random sample in [0,1]:
      let g = ctxtRand ctxt
          (sample, g') = R.random g
      put $ ctxt { ctxtRand = g' }
      render' minScale (if sample < p then c1 else c2)
      render' minScale c
      -- N.B. That's interesting. This part remained completely
      -- independent of the backend.
    (Free (ShiftRGBA r g b a c' c)) -> do
      let rgba = ctxtFill ctxt
          rgba' = clampRGBA $ ColorRGBA { colorR = colorR rgba * r
                                        , colorG = colorG rgba * g
                                        , colorB = colorB rgba * b
                                        , colorA = colorA rgba * a
                                        }
      put $ ctxt { ctxtFill = rgba' }
      -- xformAndRestore c' $ setSourceRGBA' rgba'
      render' minScale c
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
      -- xformAndRestore c' $ setSourceRGBA' rgba'
      render' minScale c
    (Free (Background r g b a c)) -> do
      -- save & restore is probably overkill here, but this should
      -- only be done once.
      -- lift $ do
      --   C.save
      --  C.setSourceRGBA r g b a
      --  C.paint
      --  C.restore
      render' minScale c
    (Free _) -> error $ "Unsupported type in renderCairo"
    (Pure _) -> return mempty
