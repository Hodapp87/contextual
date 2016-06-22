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

module Contextual ( Transform(..)
                  , Primitive(..)
                  , Instance(..)
                  , Scene(..)
                  , defaultScene
                  , updateScale
                  , sRGBA
                  ) where

import Data.Tree
import Data.Word (Word8)

import Data.Colour
import qualified Data.Colour.SRGB as SRGB

data Transform a = Scale a -- ^ Scale uniformly by the given factor.
                 | ScaleXYZ a a a -- ^ Scale non-uniformly in X, Y, and Z.g
                 | Translate a a a -- ^ Translate in X, Y, and Z.
                 | Rotate a -- ^ Rotate about origin
                 | RotateXYZ a a a -- ^ ...not really implemented
                 | Shear a a a -- ^ Shearing in X, Y, and Z (unimplemented)
                 | Fill (AlphaColour Float) -- ^ Specify a fill color
                 | Stroke (AlphaColour Float) -- ^ Specify a stroke color
                 | FillBlend Float (AlphaColour Float) -- ^ Blend fill color by given factor
                 | StrokeBlend Float (AlphaColour Float) -- ^ Blend stroke color by given factor
                   -- amount
                 deriving (Show);

-- TODO: Color transforms
-- Perhaps if I parametrize this, it will be easier to extend later to support
-- things like time-varying transformations, and relatively simple to call it
-- now just using some numerical type for 'a'.

data Primitive = Square
               | Triangle
               | Line
               | None
               deriving (Show);

-- TODO: Add more primitives (arc?)
-- Make line have a thickness? (or just let scaling handle this)

-- | An 'Instance' is either a 'Primitive', or a list of 'Transform'.
data Instance a = Prim Primitive
                | Xforms [Transform a]
                deriving (Show);
  
-- updateScale scale xforms: Given an input scale ratio and a list of
-- transforms, return a corrected scale figure (based on accumulating the
-- Scale transforms present in the list of transforms).
updateScale :: Num a => a -> [Transform a] -> a
updateScale s [] = s
updateScale s (xf:xfs) = case xf of Scale s' -> updateScale (s * s') xfs
                                    _        -> updateScale s xfs

-- | 'Scene' gives the definition of the scene. This consists of some metadata,
-- scene-wide settings, and the tree of nodes giving the automata.
data Scene = Scene {
  -- | The tree expressing the actual content of the scene
  -- TODO: Don't tie this to Float!
  tree :: Tree (Instance Float),
  -- | Human-readable scene description
  description :: String,
  -- | Background color, or Nothing if no background.
  background :: Maybe (Colour Float),
  -- | Desired top-left coordinate of viewport as (x,y). Default is (0,0).
  topLeft :: (Float, Float),
  -- | Desired bottom-right coordinate of viewport as (x,y). Default is (1,1).
  bottomRight :: (Float, Float)
  } deriving (Show);

defaultScene = Scene { tree = Node (Prim None) []
                     , description = "Default Scene"
                     , background = Nothing
                     , topLeft = (0, 0)
                     , bottomRight = (1, 1)
                     }

-- | 'sRGBA' is a convenience function to generate an 'AlphaColour' in much the
-- same way that 'Data.Colour.SRGB.sRGB' generates a 'Colour'.
sRGBA :: (Ord b, Floating b) => b -> b -> b -> b -> AlphaColour b
sRGBA r g b a = withOpacity (SRGB.sRGB r g b) a

-- Thoughts:
-- Does Haskell give me any way to process an infinite tree in such a way that
-- the processing occurs only once, and produces another infinite tree? The
-- only way offhand I can think of doing this is to start with a finite tree
-- that has references that I somehow 'close' the loop(s) in later.
-- (Later answer: Yes. This in fact a problem that comes up fairly often.
-- See Data.Reify and the paper that explains it.)
