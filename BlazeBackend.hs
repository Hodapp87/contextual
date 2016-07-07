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

This works, but is a massive kludge-pile right now - it needs some
rework in the render function to work more cleanly with the Svg monad,
and it needs to have much of its color-handling code overhauled (it
was taken from an older version of this code that relied more heavily
on Data.Colour, while the Cairo backend does not).  It generates
fairly inefficient SVGs in terms of needless numbers of groups and
transforms.

The problems I'm aware of are:

* Default stroke also does not really work right.  Right now, it
doesn't really matter because the default stroke is set to
transparent, and actually setting the stroke works.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BlazeBackend where

import Contextual hiding (scale)
import Utils

import Control.Monad.Free
import Control.Monad.State
import Data.Text.Lazy (Text)
import Data.List (intercalate)
import qualified System.Random as R

import Data.Colour
--import qualified Data.Colour.SRGB as SRGB

import Text.Blaze as Blaze
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
               intercalate " " $ map show [x0, y0, x1 - x0, y1 - y0]) $ do
            Blaze.stringComment "Generated by Contextual & BlazeBackend"
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
      -- Restore our context, but pass forward the RNG:
      g <- ctxtRand <$> get
      put $ ctxt { ctxtRand = g }
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
      r' <- render' minScale (if sample < p then c1 else c2)
      r <- render' minScale c
      return $ do
        r'
        r
    (Free (Set role color c n)) -> do
      -- Render child nodes in a modified context:
      let ctxt' = case role of Stroke -> ctxt { ctxtStroke = color }
                               Fill -> ctxt { ctxtFill = color }
      put ctxt'
      rc <- render' minScale c
      -- Restore our context, but pass forward the RNG:
      g' <- ctxtRand <$> get
      put $ ctxt { ctxtRand = g' }
      rn <- render' minScale n
      return $ do
        let color' = S.toValue $ svgColor color
            (_, _, _, alpha) = color
            attrs = case role of
              Stroke -> [ A.fill color'
                        , A.fillOpacity $ S.toValue color
                        ]
              Fill -> [ A.stroke color'
                      , A.strokeOpacity $ S.toValue alpha
                      , A.strokeWidth "0.005"
                      ]
                      -- TODO: Don't hard-code stroke width.
        S.g rc ! mconcat attrs
        rn
    (Free (Shift role view f c n)) -> do
      -- Get existing color:
      let color = case role of Stroke -> ctxtStroke ctxt
                               Fill -> ctxtFill ctxt
          
          ctxt' = case role of Stroke -> ctxt { ctxtStroke = color }
                               Fill -> ctxt { ctxtFill = color }
      -- Render child nodes in a modified context:
      put ctxt'
      rc <- render' minScale c
      -- Restore our context, but pass forward the RNG:
      g' <- ctxtRand <$> get
      put $ ctxt { ctxtRand = g' }
      rn <- render' minScale n
      return $ do
        let color' = S.toValue $ svgColor color
            (_, _, _, alpha) = color
            attrs = case role of
              Stroke -> [ A.fill color'
                        , A.fillOpacity $ S.toValue color
                        ]
              Fill -> [ A.stroke color'
                      , A.strokeOpacity $ S.toValue alpha
                      , A.strokeWidth "0.005"
                      ]
                      -- TODO: Don't hard-code stroke width.
        S.g rc ! mconcat attrs
        rn
    (Free (ShiftRGBA r g b a c' c)) -> do
      let rgba = ctxtFill ctxt
          rgba' = clampRGBA $ ColorRGBA { colorR = colorR rgba * r
                                        , colorG = colorG rgba * g
                                        , colorB = colorB rgba * b
                                        , colorA = colorA rgba * a
                                        }
      put $ ctxt { ctxtFill = rgba' }
      r' <- render' minScale c'
      -- Restore our context, but pass forward the RNG:
      g' <- ctxtRand <$> get
      put $ ctxt { ctxtRand = g' }
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
      -- Restore our context, but pass forward the RNG:
      g' <- ctxtRand <$> get
      put $ ctxt { ctxtRand = g' }
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
          A.x "-0.5" ! A.y "-0.5" ! A.width "1" ! A.height "1"
        r'
    (Free t) -> error $ "Unsupported type in blaze-svg render, " ++ show t
    (Pure _) -> return mempty
