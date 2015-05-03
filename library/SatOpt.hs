{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Main module
module SatOpt (module SatOpt) where

import           SatOpt.FFT                    as SatOpt
import           SatOpt.Render                 as SatOpt
import           SatOpt.Utility                as SatOpt
-- GENERATE: import New.Module as SatOpt

import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL
import           Data.Complex
import           Data.Packed.Matrix            (mapMatrix)
import           Numeric.LinearAlgebra.HMatrix hiding (matrix, size)
--import qualified Numeric.LinearAlgebra.HMatrix as HM

size :: Num a => a
size = 256

-- | Take a position with respect to the center and move its origin to the top right
center :: Floating a => a -> a
center x = x - (size / 2)

cnorm :: Floating a => a -> a -> a
cnorm x y = norm (center x) (center y)

window :: ℝ -> ℝ -> ℝ
window x y = (+ 0.5) $ clamp $ subtract 0.5 $ (* (size/8)) $ recip $ cnorm x y

pointSources :: [(ℝ, ℝ, ℂ)] -> ℝ -> ℝ -> ℂ
pointSources ps x y = sum $ map (\(cx, cy, s) -> s * delta cx cy x y) ps


testFunc :: ℝ -> ℝ -> ℂ
testFunc = pointSources [ (0.48, 0.48, 0.5 :+ 0)
                        , (0.52, 0.52, (-0.5) :+ 0)
                        ]

wavenum :: Matrix ℂ
wavenum = mapMatrix (const (1 :+ 0)) im

im, jm :: Matrix ℂ
im = sample2DM (\x _ -> x :+ 0) [0, 1 .. size - 1] [0, 1 .. size - 1]
jm = sample2DM (\_ y -> y :+ 0) [0, 1 .. size - 1] [0, 1 .. size - 1]

func1 :: Matrix ℂ -> Matrix ℂ
func1 x = ifft2dM $ fft2dM x / ((wavenum * wavenum) + (im * im) + (jm * jm))

eps :: ℝ
eps = 0.01

delta :: ℝ -> ℝ -> ℝ -> ℝ -> ℂ
delta cx cy x y = if norm ((x / size) - cx) ((y / size) - cy) < eps then 1 else 0

matrix :: Matrix ℂ
matrix = sample2DM testFunc [0, 1 .. size - 1] [0, 1 .. size - 1]

rendCmp :: ℂ -> (ℝ, ℝ, ℝ)
rendCmp c = uncurryRGB (\x y z -> (x, y, z)) $ hsl a 0.25 m
  where
    m = magnitude c
    a = (* (180 / pi)) $ (+ pi) $ phase c

test1, test2, test3 :: Matrix ℂ
test1 = matrix
test2 = ifft2dM $ fft2dM matrix
test3 = func1 matrix

indx :: (Indexable m v, Indexable v e) => Int -> Int -> m -> e
indx i j m = (m ! i) ! j

myfun :: ℝ -> ℝ -> (ℝ, ℝ, ℝ)
myfun i j = rendCmp $ indx (round $ i * (size - 1)) (round $ j * (size - 1)) test3

rotate :: ℂ -> ℂ
rotate = uncurry mkPolar . rot . polar
  where
    rot (m, p) = (m, p + 0.1)

visual :: (ℝ, ℝ, ℝ, ℝ) -> ℂ -> ℂ
visual (xa, xb, ya, yb) (polar -> (m, p))
  = mkPolar (((yb - ya) * (m - xa)/(xb - xa)) + ya) p

visualM :: Matrix ℂ -> Matrix ℂ
visualM cs = mapMatrix (visual (small, big, 0, 1)) cs
  where
    mags  = map magnitude $ toList $ flatten cs
    small = minimum mags
    big   = maximum mags

config :: Conf (Matrix ℂ)
config = Conf { keyBinds = const id
              , state    = test3
              , evolve   = mapMatrix rotate
              , render   = map (map rendCmp) . toLists . visualM
              , canvas   = (size, size)
              }

-- | TODO
main :: IO ()
main = runDisp config
