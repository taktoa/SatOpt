{-# LANGUAGE ViewPatterns #-}

-- | TODO
module SatOpt (module SatOpt) where

import           SatOpt.FFT               as SatOpt (fft2d, ifft2d)
import           SatOpt.Render            as SatOpt
import           SatOpt.Utility           as SatOpt
-- GENERATE: import New.Module as SatOpt

import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL
import           Data.Complex

size :: Num a => a
size = 256

center :: Floating a => a -> a
center x = x - (size / 2)

cnorm :: Floating a => a -> a -> a
cnorm x y = norm (center x) (center y)

window :: Double -> Double -> Double
window x y = (+ 0.5) $ clamp $ subtract 0.5 $ (* (size/8)) $ recip $ cnorm x y

pointSources :: [(Double, Double, Complex Double)] ->
                Double -> Double -> Complex Double
pointSources [] _ _ = 0
pointSources ps x y = sum $ map (\(cx, cy, s) -> s * delta cx cy x y) ps


testFunc :: Double -> Double -> Complex Double
testFunc = pointSources [ (0.48, 0.48, 0.5 :+ 0)
                        , (0.52, 0.52, (-0.5) :+ 0)
                        ]
-- testFunc x y = ((1 :+ 0) * delta (size / 4) (size / 4) x y) +
--                ((1 :+ 0) * delta (3 * size / 4) (3 * size / 4) x y)
--testFunc x y = (:+ 0) $ clamp $ (*2) $ recip $ cnorm x y
--testFunc x y = (f x) * (f y)
--  where
--  f a = if even $ (`div` 13) $ round $ a * 8 * size then 1 else 0
--testFunc x y = (clamp $ recip $ sqrt $ abs $ (x - y)) :+ 0
--testFunc x y = window x y * sin ((x + y) * 25 / size) :+ 0
--testFunc x y = sin ((sqrt ((x - (size / 2))^2 + (y - (size / 2))^2)) * 10 * sqrt 2 / size) :+ 0

wavenum :: [[Complex Double]]
wavenum = constm 1

im, jm :: [[Complex Double]]
im = sample2D (\x _ -> x :+ 0) [0, 1 .. size] [0, 1 .. size]
jm = sample2D (\_ y -> y :+ 0) [0, 1 .. size] [0, 1 .. size]

constm           :: Num a        =>   a   -> [[a]]
addm, subm, mulm :: Num a        => [[a]] -> [[a]] -> [[a]]
divm             :: Fractional a => [[a]] -> [[a]] -> [[a]]
negm, sqm        :: Num a        => [[a]] -> [[a]]

constm   = replicate size . replicate size
addm     = zipWith (zipWith (+))
mulm     = zipWith (zipWith (*))
divm     = zipWith (zipWith (/))
subm x y = addm x (negm y)
negm     = map (map negate)
sqm      = map (map square)

func1 :: [[Complex Double]] -> [[Complex Double]]
func1 x = ifft2d $ fft2d x `divm` (sqm wavenum `addm` sqm im `addm` sqm jm)

eps :: Double
eps = 0.01

delta :: Double -> Double -> Double -> Double -> Complex Double
delta cx cy x y = if norm ((x / size) - cx) ((y / size) - cy) < eps then 1 else 0

matrix :: [[Complex Double]]
matrix = sample2D testFunc [0, 1 .. size - 1] [0, 1 .. size - 1]

rendCmp :: Complex Double -> (Double, Double, Double)
rendCmp c = uncurryRGB (\x y z -> (x, y, z)) $ hsl a 0.25 m
  where
    m = magnitude c
    a = (* (180 / pi)) $ (+ pi) $ phase c

test1 :: [[Complex Double]]
test1 = matrix
test2 :: [[Complex Double]]
test2 = ifft2d $ fft2d matrix
test3 :: [[Complex Double]]
test3 = func1 matrix

lengthX :: [[a]] -> Int
lengthX = length . head

lengthY :: [[a]] -> Int
lengthY = length

index :: Int -> Int -> [[a]] -> a
index i j l
  | i < 0          = error $ "negative x: " ++ show i
  | j < 0          = error $ "negative y: " ++ show j
  | i >= lengthX l = error $ "x out of bounds: " ++ show i ++ " > " ++ show (lengthX l)
  | j >= lengthY l = error $ "y out of bounds: " ++ show j ++ " > " ++ show (lengthY l)
  | otherwise      = (l !! i) !! j

myfun :: Double -> Double -> (Double, Double, Double)
myfun i j = rendCmp $ index (round $ i * (size - 1)) (round $ j * (size - 1)) test3

rotate :: Complex Double -> Complex Double
rotate = uncurry mkPolar . rot . polar
  where
    rot (m, p) = (m, p + 0.1)

visual :: (Double, Double, Double, Double) -> Complex Double -> Complex Double
visual (xa, xb, ya, yb) (polar -> (m, p))
  = mkPolar (((yb - ya) * (m - xa)/(xb - xa)) + ya) p

visualM :: [[Complex Double]] -> [[Complex Double]]
visualM cs = map (map (visual (small, big, 0, 1))) cs
  where
    mags  = concatMap (map magnitude) cs
    small = minimum mags
    big   = maximum mags

config :: Conf [[Complex Double]]
config = Conf { keyBinds = const id
              , state = test3
              , evolve = map (map rotate)
              , render = map (map rendCmp) . visualM
              , canvas = (size, size)
              }

-- | TODO
main :: IO ()
main = runDisp config
