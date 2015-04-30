{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | TODO
module SatOpt (module SatOpt) where

import           SatOpt.Render as SatOpt (runDisp)
-- GENERATE: import New.Module as SatOpt

import           Data.Complex
import           Numeric.FFT   (fft, ifft)

matrix :: [[Complex Double]]
matrix = [[x :+ y | x <- [0, 1 .. 1024]] | y <- [0, 1 .. 1024]]

test1 :: [[Double]]
test1 = (\x -> unconcat 1024 $ map (log . abs . (/ (maximum x))) x) $ map magnitude $ fft $ concat matrix
test2 :: [[Double]]
test2 = (\x -> unconcat 1024 $ map (log . abs . (/ (maximum x))) x) $ map magnitude $ concat matrix

unconcat :: Int -> [a] -> [[a]]
unconcat _ [] = []
unconcat i xs = (take i xs) : unconcat i (drop i xs)

myfun :: Double -> Double -> Double
--myfun i j = (sin (20 * i)) * (sin (20 * j))
myfun i j = (test2 !! (round (i * 1023))) !! (round (j * 1023))

-- | TODO
main :: IO ()
main = runDisp (1024, 1024, myfun)
