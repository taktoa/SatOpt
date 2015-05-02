{-# LANGUAGE BangPatterns #-}

-- | TODO
module SatOpt.FFT (fft2d, ifft2d) where

import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Complex                (Complex)
import           Data.List                   (transpose)
import           Numeric.FFT                 (fft, ifft)

fft2d :: [[Complex Double]] -> [[Complex Double]]
fft2d m = transpose $ mapFFT inter
  where
    !inter = transpose $ mapFFT m
    mapFFT = parMap rdeepseq fft

ifft2d :: [[Complex Double]] -> [[Complex Double]]
ifft2d m = transpose $ mapIFFT inter
  where
    !inter = transpose $ mapIFFT m
    mapIFFT = parMap rdeepseq ifft
