{-|
Module      : Core
Description : Core module for Contextual
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

module Graphics.Rendering.Contextual.Core where

import Graphics.Rendering.Contextual.Colors

import Control.Monad.Free
import qualified Data.Colour.CIE as CIE

-- | This is the set of roles that a color might have.  Most
-- transformations that modify a color will do so based on one of
-- these roles.
data ColorRole = Stroke -- ^ The stroke at a shape's boundary
               | Fill -- ^ A shape's interior
  deriving (Show)

-- | Transformations, primitives, and other commands used (internally)
-- for specifying a grammar.
-- 
-- Backends should pattern-match on these constructors, but users
-- creating a grammar should do so via the 'Node' type and its smart
-- constructors elsewhere in this file.
data NodeF x = Square x
             | Triangle x
             | Line x
             | Scale Double Double (Node x) x
             | Translate Double Double (Node x) x
             | Rotate Double (Node x) x
             | Shear Double Double (Node x) x
             | Random Double (Node x) (Node x) x
             | Set ColorRole (LABColor Double) (Node x) x
             | Shift ColorRole ColorView Double (Node x) x
             | Background (LABColor Double) x
             deriving (Show, Functor)

-- | Main type for building a grammar.  Various smart constructors are
-- below.
type Node = Free NodeF

-- | Square of sidelength 1, center (0,0), axis-aligned
square :: Node ()
square = liftF $ Square ()

-- | Triangle of sidelength 1, center (0,0), one vertex lying along
-- positive /X/.
triangle :: Node ()
triangle = liftF $ Triangle ()

-- | Origin-centered line of length 1 along @x@ - that is, vertices at
-- (0.5, 0), (-0.5, 0).
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

-- | Set a color to some color role.
set :: ColorRole -- ^ The role of the given color
    -> LABColor Double -- ^ The color to set
    -> Node () -> Node ()
set role color c = liftF $ Set role color c ()

-- | Shift some part of a color in some role.
shift :: ColorRole
      -> ColorView
      -> Double
      -> Node () -> Node ()
shift role view f c = liftF $ Shift role view f c ()

-- | Basically 'shift', but shorter for expressing multiple
-- transformations on the same role.  Transformations @[(v1,f1),
-- (v2,f2), ...]@ go in the same order as if expanded as @shift r v1
-- f1 $ shift r v2 f2 $ ...@.
shift' :: ColorRole -> [(ColorView, Double)] -> Node () -> Node ()
shift' _ [] = id
shift' r ((v,f):l) = shift r v f . shift' r l

-- | Set the background color, including transparency.  This should
-- preferably be done early (if at all) - calling it later on or
-- multiple times will give some implementation-dependent results.
background :: LABColor Double -- ^ Color
           -> Node ()
background c = liftF $ Background c ()

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
  ("translate " ++ show dx ++ "," ++ show dy ++ " {") : rest ++ ["}"] ++
  showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (Rotate a c' c)) =
  ("rotate " ++ show a ++ " {") : rest ++ ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (Shear sx sy c' c)) =
  ("shear " ++ show sx ++ "," ++ show sy ++ " {") : rest ++
  ["}"] ++ showNode c
  where rest = indent "  " $ showNode c'
showNode (Free (Set role color c n)) =
  ("set " ++ show role ++ " " ++ show color ++ " {") : rest ++
  ["}"] ++ showNode n
  where rest = indent "  " $ showNode c
showNode (Free (Shift role view f c n)) =
  ("shift " ++ show role ++ " " ++ show view ++ " " ++ show f ++ " {") : rest
  ++ ["}"] ++ showNode n
  where rest = indent "  " $ showNode c
showNode (Free (Random p c1 c2 c)) =
  ("random p=" ++ show p ++ " {") : (indent "  " $ showNode c1) ++
  ("}, p=" ++ show (1-p) ++ " {") : (indent "  " $ showNode c2) ++ ["}"] ++
  showNode c
showNode (Free (Background color n)) =
  ("background " ++ show color) : showNode n
showNode (Free t) = error $ "Unknown type, " ++ show t

indent :: String -> [String] -> [String]
indent pfx = map (pfx ++)

clamp :: (Ord f, Num f) => f -> f
clamp v = if v < 0 then 0 else if v > 1 then 1 else v

-- | Rendering context
data Context a =
  Context { ctxtScale :: Double -- ^ Overall scale
          , ctxtFill :: LABColor Double -- ^ Current fill color
          , ctxtStroke :: LABColor Double -- ^ Current stroke color
          , ctxtRand :: a -- ^ RandomGen
          }

-- | Generate a default context, given a random number generator.
defaultContext :: r -> Context r
defaultContext rg = Context { ctxtScale = 1.0
                            , ctxtFill = (100, 0, 0, 1)
                            , ctxtStroke = (0, 0, 0, 0)
                              -- Note that changing the above stroke
                              -- doesn't actually take effect in
                              -- BlazeBackend yet.
                            , ctxtRand = rg }
