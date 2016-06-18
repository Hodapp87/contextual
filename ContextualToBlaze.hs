{-# LANGUAGE OverloadedStrings #-}
module ContextualToBlaze where

import Data.List (intercalate)
import Data.Monoid
import Data.Word (Word8)
import Data.Tree
import Numeric (showHex)

import Data.Colour
import Data.Colour.SRGB as SRGB
import Data.Colour.RGBSpace.HSL as HSL
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Contextual

render :: Contextual.Scene -> String
render scene = renderSvg $ preamble $ bg >> treeToSvg (tree scene)
  where (x0,y0) = topLeft scene
        (x1,y1) = bottomRight scene
        w = 400 * (x1 - x0)
        h = 400 * (y1 - y0)
        -- Attributes for width & height:
        w' = A.width $ S.toValue $ show w
        h' = A.height $ S.toValue $ show h
        -- Attribute for viewbox, e.g. viewbox "0 0 1 1":
        box = A.viewbox $ S.toValue $ intercalate " " $ map show [x0,y0,x1,y1]
        preamble = S.docTypeSvg ! A.version "1.1" ! w' ! h' ! box
        bg = (backgroundSvg $ background scene) ! w' ! h'

-- | 'background' generates an S.Svg element to be inserted first as
-- background color filling up the viewpoint. If the argument is Nothing, then
-- no background is generated; otherwise, it is a (R, G, B) tuple.
backgroundSvg :: Maybe (Colour Float) -> S.Svg
backgroundSvg (Just c) = S.rect ! (A.fill $ S.toValue $ SRGB.sRGB24show c)
backgroundSvg Nothing = mempty

preamble' :: S.Svg -> S.Svg
preamble' = S.docTypeSvg ! A.version "1.1" !
           A.width "400" ! A.height "400" ! A.viewbox "0 0 1 1"

tempAttribs :: S.Attribute
tempAttribs = mconcat [ --A.fill "#8080FF"
                      --, A.stroke "#404000"
                      --, A.strokeOpacity "0.50"
                      A.strokeWidth "0.005"
                      --, A.fillOpacity "0.15"
                      ]

-- | 'xformAttrVal': Turn a 'Contextual.Primitive' into 'S.Svg'.
primSvg :: Contextual.Primitive -> S.Svg
primSvg Square = S.rect ! A.width "1" ! A.height "1" !
                 (A.transform $ S.translate (-0.5) (-0.5)) ! tempAttribs
                 -- N.B. Translate (-0.5,-0.5) so it is centered at (0,0).
                 -- TODO: Get rid of tempAttribs!
primSvg Triangle = S.polygon ! (A.points $ S.toValue ptStr) ! tempAttribs
  where h = 1.0 / sqrt(3.0)
        ang n = 2 * n * pi / 3
        pt = \ang -> show (h * cos(ang)) ++ "," ++ show (h * sin(ang)) ++ " "
        ptStr = pt (ang 0) ++ pt (ang 1) ++ pt (ang 2)
primSvg None = mempty
-- TODO: Implement the rest
primSvg p@_ = error ("Unimplemented Contextual.Prim: " ++ show p)

data ColorUse = Fill' | Stroke' deriving (Show)

-- | 'getFill' turns an 'AlphaColour' into an 'S.Attribute' containing color
-- and opacity for fill or for stroke, depending on the first argument.
colorAttr :: (RealFrac a, Floating a, S.ToValue a) =>
             ColorUse -> AlphaColour a -> S.Attribute
colorAttr use alphaColor =
  let color = S.toValue $ SRGB.sRGB24show $ alphaColor `over` black
      alpha = S.toValue $ alphaChannel alphaColor
  in case use of Fill'   -> mappend (A.fill color)   (A.fillOpacity alpha)
                 Stroke' -> mappend (A.stroke color) (A.strokeOpacity alpha)

fillAttr   = colorAttr Fill'
strokeAttr = colorAttr Stroke'

-- | 'xformFn' converts a 'Transform' to a function for transforming a
-- 'TreeState'.
xformFn :: Transform Float -> TreeState -> TreeState
xformFn xform st = st'
  where addXform :: S.AttributeValue -> S.AttributeValue
        addXform av = mappend av $ xformAttr st
        addAttr :: S.Attribute -> S.Attribute
        addAttr attr = mappend attr $ attrib st
        st' = case xform of
          Scale s         -> st { scale = (scale st) * s
                                , xformAttr = addXform $ S.scale s s
                                }
          Translate x y _ -> st { xformAttr = addXform $ S.translate x y }
          Rotate x        -> st { xformAttr = addXform $ S.rotate x }
          Fill alphaColor -> st { fill = alphaColor
                                , attrib = fillAttr alphaColor
                                }
          Stroke alphaColor -> st { stroke = alphaColor
                                  , attrib = strokeAttr alphaColor
                                  }
          FillBlend fac alphaColor -> st { fill = newFill
                                         , attrib = fillAttr newFill
                                         }
            where newFill = blend fac alphaColor $ fill st
          StrokeBlend fac alphaColor -> st { stroke = newStroke
                                           , attrib = strokeAttr newStroke
                                           } 
            where newStroke = blend fac alphaColor $ stroke st
          t@_     -> error ("Unimplemented Transform: " ++ show t)

-- | 'xformsAttr' turns a list of 'Transform' and a starting 'TreeState' into
-- an 'Attribute' and an ending state.
xformsAttr :: [Transform Float] -> TreeState -> (S.Attribute, TreeState)
xformsAttr xfs state = (mappend attr xform, state')
  where state' = foldr xformFn state xfs
        attr = attrib state'
        xform = A.transform $ xformAttr state'

rgbToColor (RGB r g b) = sRGB r g b

-- | 'TreeState' tracks the state of some part of the tree during rendering.
data TreeState = TreeState {
  -- | Stroke color of the current shape
  stroke :: AlphaColour Float,
  -- | Fill color of the current shape (Should this & stroke be HSL instead?
  -- It's a question of whether I want to convert HSL to RGB for every single
  -- primitive, or convert RGB to HSL for every single transform.)
  fill :: AlphaColour Float,
  -- | A current estimate of the overall scale.  This should mainly be used
  -- for determining when to quit recursing.
  scale :: Float,
  -- | A count of how many primitives are rendered at this point.
  prims :: Int,
  -- | The current AttributeValue of the transform.
  xformAttr :: S.AttributeValue,
  -- | The current Attribute.
  attrib :: S.Attribute
  }; -- deriving (Show);

defaultState = TreeState { stroke = sRGBA 1 0 0 1
                         , fill = sRGBA 0 1 0 1
                         -- The above are purposely red and green to illuminate a bug.
                         , scale = 1.0
                         , prims = 0
                         , xformAttr = mempty
                         , attrib = mempty
                         }

type StopFn = TreeState -> Bool
-- StopFn -> State -> Tree (...) -> S.Svg -> S.Svg
-- Convenience function:  StopFn -> Tree (...) -> S.Svg

-- TODO at some point: treeToSvg_ could sensibly terminate not just on the
-- scale, but on the alpha channel being past a certain point.  However, this
-- may be a little trickier if certain paths in the tree involve increasing
-- the alpha channel.  This may start to matter when randomness is involved.
-- Being able to express termination conditions a little more generally -
-- perhaps parametrize over them - might be nice.  (The JavaScript could
-- terminate after a certain number of primitives, for instance.)

-- | 'treeToSvg_' is a scale-limited conversion of a graph to an 'S.Svg' object.
-- In this case, the scale of the graph limits the recursion; this will recurse
-- until it hits a minimum scale when converting 'graph' and appending onto
-- 'start'.
treeToSvg_ :: Float -> TreeState -> Tree (Instance Float) -> S.Svg -> S.Svg
treeToSvg_ minScale state (Node inst forest) start
  -- If we're drawing below the minimum scale, then quit recursing:
  | (scale state) < minScale = start
  | otherwise = case inst of
    -- When drawing a primitive, overall scale does not change; append the
    -- primitive, and then merge the children.
    Contextual.Prim prim ->
      foldr (treeToSvg_ minScale state) ((primSvg prim) >> start) forest
    -- To handle transforms, compute the new scale, compute all the children
    -- (passing along this new scale), and then put this all in a group that
    -- has the transformations applied.
    Contextual.Xforms xfs ->
      let (attr, state') = xformsAttr xfs state
          children = foldr (treeToSvg_ minScale state') mempty forest
      in start >> S.g children ! attr
         -- S.g is here because we cannot have multiple A.transforms on one
         -- element.

-- | 'treeToSvg' just invokes 'treeToSvg_' starting with a normal scale and
-- with an empty SVG scene, and setting the scale limit to 1e-4.
treeToSvg :: Tree (Instance Float) -> S.Svg
treeToSvg tree = treeToSvg_ 1e-4 defaultState tree mempty
