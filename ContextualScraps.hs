module ContextualScraps where

import Control.Monad.Free

data CtxtF x = Square x
             | Triangle x
             | Line x
             | Scale Float (Ctxt x) x
             deriving (Show);

instance Functor CtxtF where
  fmap f (Square    x) = Square             (f x)
  fmap f (Triangle  x) = Triangle           (f x)
  fmap f (Line      x) = Line               (f x)
  fmap f (Scale n c x) = Scale n (fmap f c) (f x)

type Ctxt = Free CtxtF

square :: Ctxt ()
square = liftF $ Square ()

triangle :: Ctxt ()
triangle = liftF $ Triangle ()

line :: Ctxt ()
line = liftF $ Line ()

scale :: Float -> Ctxt () -> Ctxt ()
scale n c = liftF $ Scale n c ()

