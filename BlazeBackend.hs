{-|
Module      : BlazeBackend
Description : Blaze (particularly blaze-svg) backend for Contextual
Copyright   : (c) Chris Hodapp, 2016
License     : ?
Maintainer  : Hodapp87@gmail.com
Stability   : experimental
Portability : POSIX

This is the backend for rendering a 'Node' grammar to an SVG via
<https://hackage.haskell.org/package/blaze-svg blaze-svg>.

-}

{-# LANGUAGE FlexibleContexts #-}

module BlazeBackend where

import Contextual hiding (scale)

import Control.Monad (when)
import Control.Monad.Free
import Control.Monad.State
import qualified System.Random as R

import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.RGBSpace.HSL as HSL

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
