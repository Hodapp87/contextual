module ContextualExamples where

import Data.Tree
import Data.Colour
import Data.Colour.SRGB

import Contextual

white = sRGB 1 1 1

spiral1 = defaultScene { description = "Square spiral thingy"
                       , background = Just white
                       , tree = spiralTree1
                       }

spiralTree1 = Node (Xforms [Translate 0 (-0.3) 0]) [spiralRec]
  where -- Recursive part (needs shifting at the end):
        spiralRec = Node (Prim None)
                    [shiftSquare, Node (Xforms xfs) [spiralRec]]
        xfs = [Translate 0.09 0 0, Scale 0.91, Rotate 6]
        -- Single shifted square that is the start:
        shiftSquare = Node (Xforms [Translate 1 1 0])
                      [Node (Prim Square) []]

spiral2 = defaultScene { description = "Triangle spiral thingy"
                       , background = Just black
                       , tree = spiralTree2
                       , bottomRight = (1.778, 1)
                       }

spiralTree2 = Node (Xforms [Scale 0.87, Translate 0.5 (-0.3) 0]) [spiralRec]
  where -- Recursive part (needs shifting at the end):
        spiralRec = Node (Prim None) [triangle, Node (Xforms xfs) [spiralRec]]
        xfs = [Translate 0.09 (-0.000) 0, Scale 0.94, Rotate 8]
        -- Single triangle:
        triangle = Node (Xforms [Translate 1 1 0])
                   [Node (Prim Triangle) []]

spiral3 = defaultScene { description = "Triangle spiral thingy, with color"
                       , background = Just black
                       , tree = spiralTree3
                       , bottomRight = (1.778, 1)
                       }

spiralTree3 = Node (Xforms [Scale 0.87, Translate 0.5 (-0.3) 0]) [spiralRec]
  where -- Recursive part (needs shifting at the end):
        spiralRec = Node (Prim None) [triangle, Node (Xforms xfs) [spiralRec]]
        xfs = [Translate 0.09 (-0.000) 0, Scale 0.94, Rotate 8]
        -- Single triangle:
        triangle = Node (Xforms [Translate 1 1 0
                                , Fill   $ sRGBA 1.0 0.3 0.3 0.2
                                , Stroke $ sRGBA 0.0 0.0 0.0 1.0
                                ])
                   [Node (Prim Triangle) []]

spiral4 = defaultScene { description = "Triangle spiral thingy, with color"
                       , background = Just white
                       , tree = spiralTree4
                       , bottomRight = (1.778, 1)
                       }

spiralTree4 = Node (Xforms [Scale 0.82, Translate 0.45 (-0.24) 0]) [spiralRec]
  where -- Recursive part (needs shifting at the end):
        spiralRec = Node (Prim None) [triangle, Node (Xforms xfs) [spiralRec]]
        xfs = [Translate 0.09 (-0.000) 0, Scale 0.94, Rotate 8]
        -- Single triangle:
        triangle = Node (Xforms [ Rotate 9
                                , Translate 1 1 0
                                , Fill   $ sRGBA 0.1 0.5 0.1 0.2
                                , Stroke $ sRGBA 0.1 0.1 0.1 1.0
                                ])
                   [Node (Prim Triangle) []]

spiral5 = defaultScene { description = "Triangle spiral thingy?"
                       , background = Just white
                       , tree = spiralTree5
                       , bottomRight = (1.778, 1)
                       }

spiralTree5 = Node (Xforms [Scale 0.82, Translate 0.45 (-0.24) 0]) [spiralRec]
  where -- Recursive part (needs shifting at the end):
        spiralRec = Node (Xforms []) [triangle, Node (Xforms xfs) [spiralRec]]
        xfs = [Translate 0.09 (-0.000) 0, Scale 0.94, Rotate 8]
        -- Single triangle:
        triangle = Node (Xforms [ Rotate 9
                                , Translate 1 1 0
                                , Fill   $ sRGBA 0.1 0.5 0.1 0.2
                                , Stroke $ sRGBA 0.1 0.1 0.1 1.0
                                ])
                   [Node (Prim Triangle) []]

spiral6 = defaultScene { description = "Triangle spiral thingy, with color shifting"
                       , background = Just white
                       , tree = spiralTree6
                       , bottomRight = (1.778, 1)
                       }

-- Note something below: The initial colors are applied *once*. The issue is
-- that when colors are applied directly to something, this is then applied at
-- every instantiation of it, and as a result, FillBlend and StrokeBlend never
-- do anything.
-- This seems sub-optimal. How would I make shapes start out as different
-- colors?

spiralTree6 = Node (Xforms init) [spiralRec]
  where init = [ Scale 0.82
               , Translate 0.45 (-0.24) 0
               , Fill   $ sRGBA 0.1 0.5 0.1 0.2
               , Stroke $ sRGBA 0.1 0.1 0.1 1.0
               ]
        -- Recursive part (needs shifting at the end):
        spiralRec = Node (Xforms []) [triangle, Node (Xforms incr) [spiralRec]]
        incr = [ Translate 0.09 (-0.000) 0
               , Scale 0.94
               , Rotate 8
               , FillBlend 1.0 $ sRGBA 0 0 0 0.2
               ]
        -- Single triangle:
        triangle = Node (Xforms [ Rotate 9
                                , Translate 1 1 0
                                ])
                   [Node (Prim Triangle) []]


simpleSpiral = Node
               (Prim Square)
               [Node (Xforms [Translate 0.09 0 0, Scale 0.91, Rotate 6]) [simpleSpiral]]

simpleSpiralFork = Node
                   (Prim Square)
                   [Node (Xforms [Translate 0.5 0.5 0, Scale 0.5, Rotate 45]) [simpleSpiralFork],
                    Node (Xforms [Translate (-0.5) 0.5 0, Scale 0.5, Rotate 45]) [simpleSpiralFork]]
