{-|
Module      : BlazeBackend
Description : Blaze (particularly blaze-svg) backend for Contextual
Copyright   : (c) Chris Hodapp, 2016
License     : ?
Maintainer  : Hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

This is the backend for rendering a 'Node' grammar to an SVG via
<https://hackage.haskell.org/package/blaze-svg blaze-svg>.  While
Cairo can export SVGs, it is a heavier dependency that may not be
available everywhere.

This works, but is a massive kludge-pile right now.  It probably also
generates fairly inefficient SVGs (in terms of needless numbers of
groups and transforms).

Default stroke also does not really work right.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BlazeBackend where

import Contextual hiding (scale)

import Control.Monad.Free
import Control.Monad.State
import Data.Text.Lazy (Text)
import Data.List (intercalate)
import qualified System.Random as R

import Data.Colour
import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.RGBSpace.HSL as HSL

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

render :: (Show a, R.RandomGen r) => r -- ^ Random generator
       -> Double -- ^ Minimum scale (below this, recursion terminates)
       -> Int -- ^ Width
       -> Int -- ^ Height
       -> Node a -- ^ Scene to render
       -> Text
render rg minScale w h node = renderSvg svg
  where (x0, x1) = (-0.5, 0.5)
        (y0, y1) = (-0.5, 0.5)
        svg = do
          -- Preamble
          S.docTypeSvg ! A.version "1.1" !
          -- Attributes for width & height:
            (A.width $ S.toValue $ show w) !
            (A.height $ S.toValue $ show h) !
            -- Attribute for viewbox, e.g. viewbox "0 0 1 1":
            (A.viewbox $ S.toValue $
              -- Note x1-x0, y1-y0; viewbox's 3rd and 4th argument are
              -- width and height, not x and y coordinates.
               intercalate " " $ map show [x0, y0, x1 - x0, y1 - y0]) $
            -- Actual scene:
            evalState (render' minScale node) $ defaultContext rg

-- Problem: I want to lift operations into S.Svg.  I can't really
-- lift, except into a StateT.  S.Svg is MarkupM (), so StateT
-- (Context a) S.Svg doesn't work (kind is wrong).  MarkupM is
-- internal to blaze, however.  I can use it, but it seems like I
-- shouldn't have to.

render' :: (Show b, R.RandomGen a) => Double -- ^ Minimum scale
        -> Node b -- ^ Starting node
        -> State (Context a) S.Svg
render' minScale node = do
  ctxt <- get
  case node of
    -- N.B. Only proceed if global scale is large enough
    (Free (Scale sx sy c' c)) -> do
      put $ ctxt { ctxtScale = ctxtScale ctxt * min sx sy }
      r' <- if (ctxtScale ctxt > minScale)
            then render' minScale c'
            else return mempty
      put $ ctxt
      r <- render' minScale c
      return $ do
        S.g r' ! (A.transform $ S.scale sx sy)
        r
    (Free (Translate dx dy c' c)) -> do
      r' <- render' minScale c'
      r <- render' minScale c
      return $ do
        S.g r' ! (A.transform $ S.translate dx dy)
        r
    (Free (Rotate a c' c)) -> do
      let degrees = a * 180 / pi
      r' <- render' minScale c'
      r <- render' minScale c
      return $ do
        S.g r' ! (A.transform $ S.rotate degrees)
        r
    (Free (Shear sx sy c' c)) -> do
      error "Shearing is not implemented for blaze-svg."
    (Free (Square c)) -> do
      r <- render' minScale c
      return $ do
        S.rect ! A.width "1" ! A.height "1" ! A.x "-0.5" ! A.y "-0.5"
        r
    (Free (Triangle c)) -> do
      let h = 1.0 / sqrt(3.0)
          ang n = 2 * n * pi / 3
          pt = \ang -> show (h * cos(ang)) ++ "," ++ show (h * sin(ang)) ++ " "
          ptStr = pt (ang 0) ++ pt (ang 1) ++ pt (ang 2)
      r <- render' minScale c
      return $ do
        S.polygon ! (A.points $ S.toValue ptStr)
        r
    (Free (Random p c1 c2 c)) -> do
      -- Get a random sample in [0,1]:
      let g = ctxtRand ctxt
          (sample, g') = R.random g
      put $ ctxt { ctxtRand = g' }
      render' minScale (if sample < p then c1 else c2)
      render' minScale c
      -- N.B. That's interesting. This part remained completely
      -- independent of the backend.
    (Free (Fill r g b a c' c)) -> do
      let rgba = ctxtFill ctxt
          rgba' = clampRGBA $ ColorRGBA { colorR = r
                                        , colorG = g
                                        , colorB = b
                                        , colorA = a
                                        }
      put $ ctxt { ctxtFill = rgba' }
      r' <- render' minScale c'
      put $ ctxt
      r1 <- render' minScale c
      return $ do
        let rgb = S.toValue $ SRGB.sRGB24show $ SRGB.sRGB r g b
        S.g r' ! (mappend (A.fill rgb) (A.fillOpacity $ S.toValue a))
        r1
    (Free (Stroke r g b a c' c)) -> do
      let rgba = ctxtStroke ctxt
          rgba' = clampRGBA $ ColorRGBA { colorR = r
                                        , colorG = g
                                        , colorB = b
                                        , colorA = a
                                        }
      put $ ctxt { ctxtStroke = rgba' }
      r' <- render' minScale c'
      put $ ctxt
      r1 <- render' minScale c
      return $ do
        let rgb = S.toValue $ SRGB.sRGB24show $ SRGB.sRGB r g b
        S.g r' ! mconcat [ A.stroke rgb
                         , A.strokeOpacity $ S.toValue a
                         , A.strokeWidth "0.005"
                         -- TODO: Don't hard-code stroke width.
                         ]
        r1
    (Free (ShiftRGBA r g b a c' c)) -> do
      let rgba = ctxtFill ctxt
          rgba' = clampRGBA $ ColorRGBA { colorR = colorR rgba * r
                                        , colorG = colorG rgba * g
                                        , colorB = colorB rgba * b
                                        , colorA = colorA rgba * a
                                        }
      put $ ctxt { ctxtFill = rgba' }
      r' <- render' minScale c'
      put $ ctxt
      r1 <- render' minScale c
      return $ do
        let rgb = S.toValue $ SRGB.sRGB24show $ SRGB.sRGB (colorR rgba') (colorG rgba') (colorB rgba')
        S.g r' ! (mappend (A.fill rgb) (A.fillOpacity $ S.toValue $ colorA rgba'))
        r1
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
      r' <- render' minScale c'
      put $ ctxt
      r1 <- render' minScale c
      return $ do
        let rgb = S.toValue $ SRGB.sRGB24show $ SRGB.sRGB (colorR rgba') (colorG rgba') (colorB rgba')
        S.g r' ! (mappend (A.fill rgb) (A.fillOpacity $ S.toValue $ colorA rgba'))
        r1
    (Free (Background r g b a c)) -> do
      r' <- render' minScale c
      return $ do
        let rgb = SRGB.sRGB r g b
        S.rect !
          (A.fill $ S.toValue $ SRGB.sRGB24show rgb) !
          A.width "2" ! A.height "2" ! (A.transform $ S.translate (-0.5) (-0.5))
          -- TODO: Get the size of this correct
        r'
    (Free t) -> error $ "Unsupported type in blaze-svg render, " ++ show t
    (Pure _) -> return mempty
