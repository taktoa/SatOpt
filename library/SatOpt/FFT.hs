{-# LANGUAGE BangPatterns #-}

-- | 2D FFT functions
module SatOpt.FFT where

import           Control.Parallel.Strategies   (parMap, rdeepseq)
import           Numeric.GSL.Fourier           (fft, ifft)
import           Numeric.LinearAlgebra.HMatrix


half [] = []
half (x:y:xs) = x : half xs

clone [] = []
clone (x:xs) = x : x : clone xs

-- | Take the FFT of a matrix of complex numbers
fft2dM :: Matrix ℂ -> Matrix ℂ
fft2dM m = fromColumns $ clone $ clone $ inter0
  where
    !inter0 = mapFFT $ inter1
    !inter1 = half $ half $ toColumns $ fromRows $ clone $ clone $ inter2
    !inter2 = mapFFT $ inter3
    !inter3 = half $ half $ toRows m
    mapFFT = parMap rdeepseq fft

-- | Take the inverse FFT of a matrix of complex numbers
ifft2dM :: Matrix ℂ -> Matrix ℂ
ifft2dM m = fromColumns $ mapIFFT $ toColumns inter
  where
    !inter = fromRows $ mapIFFT $ toRows m
    mapIFFT = parMap rdeepseq ifft
