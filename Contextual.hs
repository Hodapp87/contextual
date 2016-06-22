-- (c) 2013, Chris Hodapp
-- 2013-12-07, this is yet another rewriting of my software for drawing
-- algorithmic images based on a context-free grammar, eventually a
-- nondeterministic one.
-- This was inspired by Context Free: http://www.contextfreeart.org/
-- It was eventually rewritten partially in a few other languages as practice.
-- JavaScript, HTML5, Canvas/SVG:
--    https://github.com/Hodapp87/html5-dabbling/tree/master/contextual
-- Scala: https://github.com/Hodapp87/scala_cf3
-- Clojure: https://github.com/Hodapp87/contextual

module Contextual where

import Control.Monad.Free

data CtxtF x = Square x
             | Triangle x
             | Line x
             | Scale Double (Ctxt x) x
             | Translate Double Double (Ctxt x) x
             | Rotate Double (Ctxt x) x
             deriving (Show);

type Ctxt = Free CtxtF

instance Functor CtxtF where
  fmap f (Square            x) = Square                     $ f x
  fmap f (Triangle          x) = Triangle                   $ f x
  fmap f (Line              x) = Line                       $ f x
  fmap f (Scale n c         x) = Scale n (fmap f c)         $ f x
  fmap f (Translate dx dy c x) = Translate dx dy (fmap f c) $ f x
  fmap f (Rotate a c        x) = Rotate a (fmap f c)        $ f x

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

-- | Rotation by the given angle (in radians); positive angles
-- correspond to a rotation from positive X axis to positive Y axis.
rotate :: Double -> Ctxt () -> Ctxt ()
rotate a c = liftF $ Rotate a c ()

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
  -- TODO: Remove some of the boilerplate for all the transformations above
showCtxt (Free t@_) = error $ "Unknown type, " ++ show t

indent :: String -> [String] -> [String]
indent pfx = map (pfx ++)
