{-# LANGUAGE BangPatterns #-}

-- | TODO
module SatOpt.FFT where

import           Control.Parallel.Strategies   (parMap, rdeepseq)
--import           Data.Complex                  (Complex)
import           Data.List                     (transpose)
import           Numeric.FFT                   (fft, ifft)
import           Numeric.LinearAlgebra.HMatrix

fft2d, ifft2d :: [[ℂ]] -> [[ℂ]]
fft2d m = transpose $ mapFFT inter
  where
    !inter = transpose $ mapFFT m
    mapFFT = parMap rdeepseq fft
ifft2d m = transpose $ mapIFFT inter
  where
    !inter = transpose $ mapIFFT m
    mapIFFT = parMap rdeepseq ifft

fftV :: Vector ℂ -> Vector ℂ
fftV = fromList . fft . toList

ifftV :: Vector ℂ -> Vector ℂ
ifftV = fromList . ifft . toList

fft2dM :: Matrix ℂ -> Matrix ℂ
fft2dM m = fromColumns $ mapFFT $ toColumns inter
  where
    !inter = fromRows $ mapFFT $ toRows m
    mapFFT = parMap rdeepseq fftV

ifft2dM :: Matrix ℂ -> Matrix ℂ
ifft2dM m = fromColumns $ mapIFFT $ toColumns inter
  where
    !inter = fromRows $ mapIFFT $ toRows m
    mapIFFT = parMap rdeepseq ifftV
