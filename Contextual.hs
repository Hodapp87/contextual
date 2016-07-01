{-|
Module      : CairoBackend
Description : Cairo backend for Contextual
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

module Contextual where

import Control.Monad.Free

data NodeF x = Square x
             | Triangle x
             | Line x
             | Scale Double (Node x) x
             | Translate Double Double (Node x) x
             | Rotate Double (Node x) x
             | Shear Double Double (Node x) x
             | ShiftRGBA Double Double Double Double (Node x) x
             | ShiftHSL Double Double Double Double (Node x) x
             | Random Double (Node x) (Node x) x
             deriving (Show);

type Node = Free NodeF

instance Functor NodeF where
  fmap f (Square x) = Square $ f x
  fmap f (Triangle x) = Triangle $ f x
  fmap f (Line x) = Line $ f x
  fmap f (Scale n c x) = Scale n (fmap f c) $ f x
  fmap f (Translate dx dy c x) = Translate dx dy (fmap f c) $ f x
  fmap f (Rotate a c x) = Rotate a (fmap f c) $ f x
  fmap f (Shear sx sy c x) = Shear sx sy (fmap f c) $ f x
  fmap f (ShiftRGBA r g b a c x) = ShiftRGBA r g b a (fmap f c) $ f x
  fmap f (ShiftHSL dh s l a c x) = ShiftHSL dh s l a (fmap f c) $ f x
  fmap f (Random p c1 c2 x) = Random p (fmap f c1) (fmap f c2) $ f x

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

-- | Uniform scaling in X & Y of the surrounded 'Node'
scale :: Double -> Node () -> Node ()
scale n c = liftF $ Scale n c ()

-- | Translation in X & Y of the surrounded 'Node'
translate :: Double -> Double -> Node () -> Node ()
translate dx dy c = liftF $ Translate dx dy c ()

-- | Rotate the 'Node' by the given angle (in radians); positive
-- angles correspond to a rotation from positive X axis to positive Y
-- axis.
rotate :: Double -> Node () -> Node ()
rotate a c = liftF $ Rotate a c ()

-- | Shear the surrounded 'Node' with the given X and Y coefficients.
shear :: Double -> Double -> Node () -> Node ()
shear sx sy c = liftF $ Shear sx sy c ()

-- | Shift the color by the given factors - which apply to red, green,
-- blue, and alpha, respectively.  A value of 1 leaves that channel
-- unchanged.
shiftRGBA :: Double -- ^ Red factor
          -> Double -- ^ Green factor
          -> Double -- ^ Blue factor
          -> Double -- ^ Alpha (transparency) factor
          -> Node () -> Node ()
shiftRGBA r g b a c = liftF $ ShiftRGBA r g b a c ()

-- | Shift the color in HSL (hue, saturation, lightness) space.  The
-- first parameter is a delta, in degrees, added to hue (thus a value
-- of 0 is neutral).  The remaining parameters are factors applied to
-- saturation, value, and alpha, respectively (and as with 'shiftRGBA'
-- a value of 1 is neutral).
shiftHSL :: Double -- ^ Hue delta (degrees)
         -> Double -- ^ Saturation factor
         -> Double -- ^ Lightness factor
         -> Double -- ^ Alpha (transparency) factor
         -> Node () -> Node ()
shiftHSL dh s l a c = liftF $ ShiftHSL dh s l a c ()

-- | Randomly select a child context.  The first argument 'p' gives
-- the probability that the first child is selected, and the second is
-- selected with probability '(1-p)'.
random :: Double -> Node () -> Node () -> Node ()
random p c1 c2 = liftF $ Random p c1 c2 ()

-- | Pretty-print a 'Node'
showNode :: (Show a) => Node a -> [String]
showNode (Pure _) = []
showNode (Free (Square c)) = "square" : showNode c
showNode (Free (Triangle c)) = "triangle" : showNode c
showNode (Free (Line c)) = "line" : showNode c
showNode (Free (Scale n c' c)) =
  ("scale " ++ show n ++ " {") : rest ++ ["}"] ++ showNode c
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
showNode (Free (Random p c1 c2 c)) =
  ("random p=" ++ show p ++ " {") : (indent "  " $ showNode c1) ++
  ("}, p=" ++ show (1-p) ++ " {") : (indent "  " $ showNode c2) ++ ["}"] ++
  showNode c
showNode (Free t@_) = error $ "Unknown type, " ++ show t

indent :: String -> [String] -> [String]
indent pfx = map (pfx ++)
