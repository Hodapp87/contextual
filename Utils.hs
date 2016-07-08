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

import Text.Printf (printf, PrintfArg)

import qualified Data.Colour.CIE as CIE
import qualified Data.Colour.CIE.Illuminant as Illuminant
import qualified Data.Colour.RGBSpace as RGBSpace
import qualified Data.Colour.RGBSpace.HSL as HSL
import qualified Data.Colour.SRGB as SRGB

-- | What we use internally for colors: a tuple with (\L*\, \a*\,
-- \b*\, alpha); first three parts from CIELAB colorspace, last part
-- just normal transparency.
type LABColor a = (a, a, a, a)

-- | This is the aspect of the color that shifted, or otherwise
-- modified.  All operations refer to CIELAB or CIELCh colorspace, and
-- so terms like hue, saturation, and brightness do not map exactly to
-- HSV or HSL colorspace.
data ColorView = Lum -- ^ Luminance (brightness), that is, the /L/
                 -- from CIELAB and CIELCh
               | Chrom -- ^ Chrome (relative saturation or purity);
                       -- more explicitly, the /C*/ from CIELCh.
               | Alpha -- ^ Alpha (transparency)
               | Hue -- ^ Hue angle; more explicitly, the /h°/ from
                     -- CIELCh.
               | LAB_a -- ^ @a*@ opponent color axis from CIELAB
                       -- (loosely, positive is redness and negative
                       -- is yellowness)
               | LAB_b -- ^ @b*@ opponent color axis from CIELAB
                       -- (loosely, positive is yellowness and
                       -- negative is blueness)
  deriving (Show)

-- | Convert CIE LAB to (/L*/, /C*/, /h°/).
lab2lch :: RealFloat a => LABColor a -> (a, a, a)
lab2lch (l, a, b, _) = (l, sqrt (a*a + b*b), atan2 b a)

-- | Shift a given view of a 'LABColor' by some amount.
shiftColor :: RealFloat a => ColorView
           -> a -- ^ Amount by which to shift this view of the color
           -> LABColor a -> LABColor a
shiftColor Lum f (l, a, b, alpha) = (l + f, a, b, alpha)
shiftColor Chrom f c@(l, a, b, alpha) = (l, a', b', alpha)
  where (l, chrom, hue) = lab2lch c
        a' = (chrom + f) * cos hue
        b' = (chrom + f) * sin hue
shiftColor Alpha f (l, a, b, alpha) = (l, a, b, alpha + f) -- correct?
shiftColor Hue f c@(l, a, b, alpha) = (l, a', b', alpha)
  where (l, chrom, hue) = lab2lch c
        a' = chrom * cos (hue + f)
        b' = chrom * sin (hue + f)
shiftColor LAB_a f (l, a, b, alpha) = (l, a + f, b, alpha)
shiftColor LAB_b f (l, a, b, alpha) = (l, a, b + f, alpha)
-- I'm not sure if additive is the proper way to treat these.

-- | Convert a 'LABColor', ignoring transparency, to an SVG color.
-- This will be of a format like @#CD853F cielab(62.253188, 23.950124,
-- 48.410653)@, which includes both the CIELAB color value and an sRGB
-- fallback (for which D65 whitepoint is assumed).
--
-- See <https://www.w3.org/TR/SVGColor12/#LAB>.
svgColor ::
  (PrintfArg a, Ord a, Floating a, RealFrac a) => LABColor a -> String
svgColor labColor = printf "%s cielab(%f, %f, %f)" rgbStr l a b
  where (l, a, b, _) = labColor
        -- Compute the fallback color in sRGB with D65 whitepoint:
        color = CIE.cieLAB Illuminant.d65 l a b
        rgbStr = SRGB.sRGB24show color
