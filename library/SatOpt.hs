{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
size = 256

matrix :: [[Complex Double]]
matrix = [[sin (x / 16) * sin (y / 16) :+ 0 | x <- [0, 1 .. size]] | y <- [0, 1 .. size]]

fft2d :: [[Complex Double]] -> [[Complex Double]]
fft2d m = transpose $ mapFFT $ transpose $ mapFFT m
  where
    mapFFT = parMap rdeepseq fft

rendCmp :: Complex Double -> (Double, Double, Double)
rendCmp c = uncurryRGB (\x y z -> (x, y, z)) $ hsl a 1 m
  where
    m = magnitude c
    a = (* (180 / pi)) $ (+ pi) $ phase c

test1 :: [[Complex Double]]
test1 = parMap rdeepseq fft matrix
test2 :: [[Complex Double]]
test2 = matrix

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
