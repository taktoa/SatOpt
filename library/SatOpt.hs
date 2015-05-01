{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE BangPatterns #-}

-- | TODO
module SatOpt (module SatOpt) where

import           SatOpt.Render               as SatOpt (runDisp)
-- GENERATE: import New.Module as SatOpt

import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL
import           Data.Complex
import           Data.List                   (transpose)
import           Debug.Trace                 (trace)
import           Numeric.FFT                 (fft, ifft)

size :: Num a => a
size = 1024

center :: Floating a => a -> a
center x = x - (size / 2)

norm :: Floating a => a -> a -> a
norm x y = sqrt $ x^2 + y^2

cnorm :: Floating a => a -> a -> a
cnorm x y = norm (center x) (center y)

clamp :: Double -> Double
clamp x
  | x > 1     = 1
  | x < 0     = 0
  | otherwise = x

window :: Double -> Double -> Double
window x y = (+ 0.5) $ clamp $ subtract 0.5 $ (* (size/8)) $ recip $ cnorm x y

square :: Num a => a -> a
square x = x * x

--fracComp :: RealFrac a => a -> a

testFunc :: Double -> Double -> Complex Double
testFunc x y = (f x) * (f y)
  where
  f a = if even $ (`div` 13) $ round $ a * 8 * size then 1 else 0
--testFunc x y = (clamp $ recip $ sqrt $ abs $ (x - y)) :+ 0
--testFunc x y = window x y * sin ((x + y) * 25 / size) :+ 0
--testFunc x y = sin ((sqrt ((x - (size / 2))^2 + (y - (size / 2))^2)) * 10 * sqrt 2 / size) :+ 0

matrix :: [[Complex Double]]
matrix = [[testFunc x y | y <- [0, 1 .. size]] | x <- [0, 1 .. size]]

fft2d :: [[Complex Double]] -> [[Complex Double]]
fft2d m = transpose $ mapFFT $ inter
  where
    !inter = transpose $ mapFFT m
    mapFFT = parMap rdeepseq fft

ifft2d :: [[Complex Double]] -> [[Complex Double]]
ifft2d m = transpose $ mapIFFT $ transpose $ mapIFFT m
  where
    mapIFFT = parMap rdeepseq ifft

rendCmp :: Complex Double -> (Double, Double, Double)
rendCmp c = uncurryRGB (\x y z -> (x, y, z)) $ hsl a 1 m
  where
    m = magnitude c
    a = (* (180 / pi)) $ (+ pi) $ phase c

test1 :: [[Complex Double]]
test1 = matrix
test2 :: [[Complex Double]]
test2 = fft2d matrix
test3 :: [[Complex Double]]
test3 = ifft2d matrix

unconcat :: Int -> [a] -> [[a]]
unconcat _ [] = []
unconcat i xs = (take i xs) : unconcat i (drop i xs)

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
      --trace ("i: " ++ show i ++ ", j: " ++ show j) $ (l !! i) !! j

myfun :: Double -> Double -> (Double, Double, Double)
--myfun i j = (sin (20 * i)) * (sin (20 * j))
myfun i j = rendCmp $ index (round $ i * (size - 1)) (round $ j * (size - 1)) test2
  --(test1 !! (round (i * (size - 2)))) !! (round (j * (size - 2)))

-- | TODO
main :: IO ()
main = runDisp (size, size, myfun)
