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

data CtxtF x = Square                           x
             | Triangle                         x
             | Line                             x
             | Scale Double            (Ctxt x) x
             | Translate Double Double (Ctxt x) x
             | Rotate Double           (Ctxt x) x
             | Shear Double Double     (Ctxt x) x
             deriving (Show);

type Ctxt = Free CtxtF

instance Functor CtxtF where
  fmap f (Square            x) = Square                     $ f x
  fmap f (Triangle          x) = Triangle                   $ f x
  fmap f (Line              x) = Line                       $ f x
  fmap f (Scale n c         x) = Scale n (fmap f c)         $ f x
  fmap f (Translate dx dy c x) = Translate dx dy (fmap f c) $ f x
  fmap f (Rotate a c        x) = Rotate a (fmap f c)        $ f x
  fmap f (Shear sx sy c     x) = Shear sx sy (fmap f c)     $ f x

-- | Square of sidelength 1, center (0,0), axis-aligned
square :: Ctxt ()
square = liftF $ Square ()

-- | Not implemented!
triangle :: Ctxt ()
triangle = liftF $ Triangle ()

-- | Not implemented!
line :: Ctxt ()
line = liftF $ Line ()

-- | Uniform scaling in X & Y of the surrounded 'Ctxt'
scale :: Double -> Ctxt () -> Ctxt ()
scale n c = liftF $ Scale n c ()

-- | Translation in X & Y of the surrounded 'Ctxt'
translate :: Double -> Double -> Ctxt () -> Ctxt ()
translate dx dy c = liftF $ Translate dx dy c ()

-- | Rotate the 'Ctxt' by the given angle (in radians); positive
-- angles correspond to a rotation from positive X axis to positive Y
-- axis.
rotate :: Double -> Ctxt () -> Ctxt ()
rotate a c = liftF $ Rotate a c ()

-- | Shear the surrounded 'Ctxt' with the given X and Y coefficients.
shear :: Double -> Double -> Ctxt () -> Ctxt ()
shear sx sy c = liftF $ Shear sx sy c ()

-- | Pretty-print a 'Ctxt'
showCtxt :: (Show a) => Ctxt a -> [String]
showCtxt (Pure _) = []
showCtxt (Free (Square c)) = "square" : showCtxt c
showCtxt (Free (Triangle c)) = "triangle" : showCtxt c
showCtxt (Free (Line c)) = "line" : showCtxt c
showCtxt (Free (Scale n c' c)) =
  ("scale " ++ show n ++ " {") : rest ++ ["}"] ++ showCtxt c
  where rest = indent "  " $ showCtxt c'
showCtxt (Free (Translate dx dy c' c)) =
  ("translate " ++ show dx ++ "," ++ show dy ++ " {") : rest ++ ["}"] ++ showCtxt c
  where rest = indent "  " $ showCtxt c'
showCtxt (Free (Rotate a c' c)) =
  ("rotate " ++ show a ++ " {") : rest ++ ["}"] ++ showCtxt c
  where rest = indent "  " $ showCtxt c'
showCtxt (Free (Shear sx sy c' c)) =
  ("shear " ++ show sx ++ "," ++ show sy ++ " {") : rest ++ ["}"] ++ showCtxt c
  where rest = indent "  " $ showCtxt c'
  -- TODO: Remove some of the boilerplate for all the transformations above
showCtxt (Free t@_) = error $ "Unknown type, " ++ show t

indent :: String -> [String] -> [String]
indent pfx = map (pfx ++)
