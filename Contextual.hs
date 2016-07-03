{-|
Module      : Contextual
Description : Main module for Contextual
Copyright   : (c) Chris Hodapp, 2016
License     : ?
Maintainer  : Hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

This is yet another rewriting of my software for drawing
algorithmic images based on a context-free grammar, eventually a
nondeterministic one.

This was inspired by <http://www.contextfreeart.org/ Context Free>,
and a few other tools that I wrote to reimplement Context Free for
practice, such as
<https://github.com/Hodapp87/html5-dabbling/tree/master/contextual>
for an implementation in JavaScript, HTML5, and Canvas/SVG,
<https://github.com/Hodapp87/scala_cf3 scala_cf5> for a Scala
implementation, and <https://github.com/Hodapp87/contextual_clojure
contextual_clojure> for a Clojure one.

-}

{-# LANGUAGE DeriveFunctor #-}

module Contextual where

import Control.Monad.Free
import qualified Data.Colour.SRGB as SRGB

data NodeF x = Square x
             | Triangle x
             | Line x
             | Scale Double Double (Node x) x
             | Translate Double Double (Node x) x
             | Rotate Double (Node x) x
             | Shear Double Double (Node x) x
             | Random Double (Node x) (Node x) x
             | Stroke Double Double Double Double (Node x) x
             | Fill Double Double Double Double (Node x) x
             | ShiftRGBA Double Double Double Double (Node x) x
             | ShiftHSL Double Double Double Double (Node x) x
             | Background Double Double Double Double x
             deriving (Show, Functor);

type Node = Free NodeF

-- | Square of sidelength 1, center (0,0), axis-aligned
square :: Node ()
square = liftF $ Square ()

-- | Triangle of sidelength 1, center (0,0), one vertex lying along
-- positive X.
triangle :: Node ()
triangle = liftF $ Triangle ()

-- | Not implemented!
line :: Node ()
line = liftF $ Line ()

-- | Uniform scaling in X & Y of the child 'Node'
scale :: Double -> Node () -> Node ()
scale n c = liftF $ Scale n n c ()

-- | Separate scaling in X & Y of the child 'Node'
scale2 :: Double -- ^ X scale
       -> Double -- ^ Y scale
       -> Node () -> Node ()
scale2 sx sy c = liftF $ Scale sx sy c ()

-- | Translation in X & Y of the child 'Node'
translate :: Double -> Double -> Node () -> Node ()
translate dx dy c = liftF $ Translate dx dy c ()

-- | Rotate the 'Node' by the given angle (in radians); positive
-- angles correspond to a rotation from positive X axis to positive Y
-- axis.
rotate :: Double -> Node () -> Node ()
rotate a c = liftF $ Rotate a c ()

-- | Shear the child 'Node' with the given X and Y coefficients.
shear :: Double -> Double -> Node () -> Node ()
shear sx sy c = liftF $ Shear sx sy c ()

-- | Randomly select a child context.  The first argument 'p' gives
-- the probability that the first child is selected, and the second is
-- selected with probability '(1-p)'.
random :: Double -> Node () -> Node () -> Node ()
random p c1 c2 = liftF $ Random p c1 c2 ()

-- | Set stroke color (ignoring any 'inherited' stroke color, regardless
-- of alpha) on the child 'Node'.
stroke :: Double -- ^ Red
     -> Double -- ^ Green
     -> Double -- ^ Blue
     -> Double -- ^ Alpha (transparency)
     -> Node () -> Node ()
stroke r g b a c = liftF $ Stroke r g b a c ()

-- TODO: If stroke and fill are that close perhaps I can just make one
-- 'set color' constructor/transform, and make stroke/fill a parameter.

-- | Set fill color (ignoring any 'inherited' fill color, regardless
-- of alpha) on the child 'Node'.
fill :: Double -- ^ Red
     -> Double -- ^ Green
     -> Double -- ^ Blue
     -> Double -- ^ Alpha (transparency)
     -> Node () -> Node ()
fill r g b a c = liftF $ Fill r g b a c ()

-- | Shift the fill color by the given factors - which apply to red,
-- green, blue, and alpha, respectively.  A value of 1 leaves that
-- channel unchanged.
shiftRGBA :: Double -- ^ Red factor
          -> Double -- ^ Green factor
          -> Double -- ^ Blue factor
          -> Double -- ^ Alpha (transparency) factor
          -> Node () -> Node ()
shiftRGBA r g b a c = liftF $ ShiftRGBA r g b a c ()

-- | Shift the fill color in HSL (hue, saturation, lightness) space.
-- The first parameter is a delta, in degrees, added to hue (thus a
-- value of 0 is neutral).  The remaining parameters are factors
-- applied to saturation, value, and alpha, respectively (and as with
-- 'shiftRGBA' a value of 1 is neutral).
shiftHSL :: Double -- ^ Hue delta (degrees)
         -> Double -- ^ Saturation factor
         -> Double -- ^ Lightness factor
         -> Double -- ^ Alpha (transparency) factor
         -> Node () -> Node ()
shiftHSL dh s l a c = liftF $ ShiftHSL dh s l a c ()

-- | Set the background color, with all components in range [0-1].
-- This should preferably be done early (if at all) - calling it later
-- on or multiple times will give some implementation-dependent
-- results.
background :: Double -- ^ Red
           -> Double -- ^ Green
           -> Double -- ^ Black
           -> Double -- ^ Alpha (transparency)
           -> Node ()
background r g b a = liftF $ Background r g b a ()

-- | Pretty-print a 'Node'
showNode :: (Show a) => Node a -> [String]
showNode (Pure _) = []
showNode (Free (Square c)) = "square" : showNode c
showNode (Free (Triangle c)) = "triangle" : showNode c
showNode (Free (Line c)) = "line" : showNode c
showNode (Free (Scale sx sy c' c)) =
  ("scale " ++ show sx ++ "," ++ show sy ++ " {") : rest ++ ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (Translate dx dy c' c)) =
  ("translate " ++ show dx ++ "," ++ show dy ++ " {") : rest ++ ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (Rotate a c' c)) =
  ("rotate " ++ show a ++ " {") : rest ++ ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (Shear sx sy c' c)) =
  ("shear " ++ show sx ++ "," ++ show sy ++ " {") : rest ++ ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (ShiftRGBA r g b a c' c)) =
  ("shiftRGBA " ++ show r ++ "," ++ show g ++ "," ++ show b ++
   "," ++ show a ++ " {") : rest ++ ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (ShiftHSL dh s l a c' c)) =
  ("shiftHSL " ++ show dh  ++ "," ++ show s ++ "," ++ show l ++
   "," ++ show a ++ " {") : rest ++ ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
  -- TODO: Remove some of the boilerplate for all the transformations above
showNode (Free (Fill r g b a c' c)) =
  ("fill " ++ show r ++ "," ++ show g ++ "," ++ show b ++
   "," ++ show a ++ " {") : rest ++ ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (Stroke r g b a c' c)) =
  ("stroke " ++ show r ++ "," ++ show g ++ "," ++ show b ++
   "," ++ show a ++ " {") : rest ++ ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (Random p c1 c2 c)) =
  ("random p=" ++ show p ++ " {") : (indent "  " $ showNode c1) ++
  ("}, p=" ++ show (1-p) ++ " {") : (indent "  " $ showNode c2) ++ ["}"] ++
  showNode c
showNode (Free (Background r g b a c)) =
  ("background " ++ show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show a) :
  showNode c
showNode (Free t) = error $ "Unknown type, " ++ show t

indent :: String -> [String] -> [String]
indent pfx = map (pfx ++)

clamp :: (Ord f, Num f) => f -> f
clamp v = if v < 0 then 0 else if v > 1 then 1 else v

-- | Simple color type, probably going away at some point.  All
-- components should be in the range [0,1].
data ColorRGBA = ColorRGBA { colorR :: Double
                           , colorG :: Double
                           , colorB :: Double
                           , colorA :: Double
                           } deriving Show



-- | Clamp a 'ColorRGBA' so all colors are within [0,1].
clampRGBA :: ColorRGBA -> ColorRGBA
clampRGBA rgba = ColorRGBA { colorR = clamp $ colorR rgba
                           , colorG = clamp $ colorG rgba
                           , colorB = clamp $ colorB rgba
                           , colorA = clamp $ colorA rgba
                           }

-- | Rendering context
data Context a = Context { ctxtScale :: Double -- ^ Overall scale
                         , ctxtFill :: ColorRGBA -- ^ Current fill color
                         , ctxtStroke :: ColorRGBA -- ^ Current stroke color
                         , ctxtRand :: a -- ^ RandomGen
                         }

-- | Generate a default context, given a random number generator.
defaultContext :: r -> Context r
defaultContext rg = Context { ctxtScale = 1.0
                            , ctxtFill = ColorRGBA 0.0 0.0 0.0 1.0
                            , ctxtStroke = ColorRGBA 0.0 0.0 0.0 1.0
                            , ctxtRand = rg }
