{-|
Module      : Utils
Description : 
Copyright   : (c) Chris Hodapp, 2016
License     : ?
Maintainer  : Hodapp87@gmail.com
Stability   : experimental
Portability : POSIX


-}

module Utils where

import Text.Printf (printf)

import qualified Data.Colour.CIE as CIE
import qualified Data.Colour.CIE.Illuminant as Illuminant
import qualified Data.Colour.RGBSpace as RGBSpace
import qualified Data.Colour.RGBSpace.HSL as HSL
import qualified Data.Colour.SRGB as SRGB

import Contextual

-- | Convert a 'LABColor', ignoring transparency, to an SVG color.
-- This will be of a format like @#CD853F cielab(62.253188, 23.950124,
-- 48.410653)@, which includes both the CIELAB color value and an sRGB
-- fallback.
svgColor :: LABColor -> String
svgColor labColor = printf "%s cielab(%f, %f, %f)" rgbStr l a b
  where (l, a, b, _) = labColor
        -- Compute the fallback color in sRGB with D65 whitepoint:
        color = CIE.cieLAB Illuminant.d65 l a b
        -- rgb = RGBSpace.toRGBUsingSpace SRGB.sRGBSpace color
        rgbStr = SRGB.sRGB24show color
