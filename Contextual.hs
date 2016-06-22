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

data CtxtF x = Square x -- ^ Square of sidelength 1, center (0,0),
                        -- axis-aligned
             | Triangle x -- ^ Not implemented!
             | Line x -- ^ Not implemented!
             | Scale Double (Ctxt x) x -- ^ Uniform scaling in X & Y
                                       -- of the surrounded 'Ctxt'
             | Translate Double Double (Ctxt x) x -- ^ Translation in
                                                  -- X & Y of the
                                                  -- surrounded 'ctxt'
             deriving (Show);

type Ctxt = Free CtxtF

instance Functor CtxtF where
  fmap f (Square    x)         = Square             (f x)
  fmap f (Triangle  x)         = Triangle           (f x)
  fmap f (Line      x)         = Line               (f x)
  fmap f (Scale n c x)         = Scale n (fmap f c) (f x)
  fmap f (Translate dx dy c x) = Translate dx dy (fmap f c) (f x)

square :: Ctxt ()
square = liftF $ Square ()

triangle :: Ctxt ()
triangle = liftF $ Triangle ()

line :: Ctxt ()
line = liftF $ Line ()

scale :: Double -> Ctxt () -> Ctxt ()
scale n c = liftF $ Scale n c ()

translate :: Double -> Double -> Ctxt () -> Ctxt ()
translate dx dy c = liftF $ Translate dx dy c ()

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
showCtxt (Free t@_) = error $ "Unknown type, " ++ show t

indent :: String -> [String] -> [String]
indent pfx = map (pfx ++)
