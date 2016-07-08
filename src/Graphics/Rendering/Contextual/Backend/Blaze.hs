{-|
Module      : Blaze
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

module Graphics.Rendering.Contextual.Backend.Blaze where

import Graphics.Rendering.Contextual.Core hiding (scale)
import Graphics.Rendering.Contextual.Colors

import Control.Monad.Free
import Control.Monad.State
import Data.Text.Lazy (Text)
import Data.List (intercalate)
import qualified System.Random as R
import Text.Printf (PrintfArg)

import Data.Colour

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
      -- Restore our context, except for the RNG:
      modify $ \c -> ctxt { ctxtRand = ctxtRand c }
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
      put $ case role of Stroke -> ctxt { ctxtStroke = color }
                         Fill   -> ctxt { ctxtFill = color }
      rc <- render' minScale c
      -- Restore our context, except for the RNG:
      modify $ \c -> ctxt { ctxtRand = ctxtRand c }
      rn <- render' minScale n
      return (svgColorGroup role color rc >> rn)
    (Free (Shift role view f c n)) -> do
      -- Get existing color and transform it:
      let color = case role of Stroke -> ctxtStroke ctxt
                               Fill -> ctxtFill ctxt
          color' = shiftColor view f color
      -- Render child nodes in a modified context:
      put $ case role of Stroke -> ctxt { ctxtStroke = color' }
                         Fill   -> ctxt { ctxtFill = color' }
      rc <- render' minScale c
      -- Restore our context, except for the RNG:
      modify $ \c -> ctxt { ctxtRand = ctxtRand c }
      rn <- render' minScale n
      return (svgColorGroup role color rc >> rn)
        -- TODO: Most of above is identical with the 'Set' case; put
        -- it in one place
    (Free (Background color@(_,_,_,alpha) c)) -> do
      r' <- render' minScale c
      return $ do
        S.rect !
          (A.fill $ S.toValue $ svgColor color) !
          (A.fillOpacity $ S.toValue alpha) !
          A.x "-0.5" ! A.y "-0.5" ! A.width "1" ! A.height "1"
        r'
    (Free t) -> error $ "Unsupported type in blaze-svg render, " ++ show t
    (Pure _) -> return mempty

-- | Put the given 'S.Svg' element inside a group which has stroke or
-- fill attributes (including transparency) for the given color and
-- role.
svgColorGroup :: ColorRole -> LABColor Double -> S.Svg -> S.Svg
svgColorGroup role color svg =
  let color' = S.toValue $ svgColor color
      (_, _, _, alpha) = color
      attrs = case role of
        Fill -> [ A.fill color'
                , A.fillOpacity $ S.toValue alpha
                ]
        Stroke -> [ A.stroke color'
                  , A.strokeOpacity $ S.toValue alpha
                  , A.strokeWidth "0.005"
                  ]
                  -- TODO: Don't hard-code stroke width.
    in S.g svg ! mconcat attrs
