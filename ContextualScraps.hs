module ContextualScraps where

import Control.Monad.Free

data CtxtF x = Square x
             | Triangle x
             | Line x
             | Scale Double (Ctxt x) x
             | Translate Double Double (Ctxt x) x
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
