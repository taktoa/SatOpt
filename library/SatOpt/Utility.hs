{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | TODO
module SatOpt.Utility where

import           Data.Packed.Matrix            (buildMatrix, liftMatrix2)
import           Data.Packed.Vector            (dim, zipVectorWith)
import           Numeric.LinearAlgebra.Data
import           Numeric.LinearAlgebra.HMatrix

zipMatrixWith ::(Element a, Element b, Element c)
                => (a -> b -> c)
                -> Matrix a
                -> Matrix b
                -> Matrix c
zipMatrixWith f = liftMatrix2 (zipVectorWith f)

sample2D :: (a -> a -> b) -> [a] -> [a] -> [[b]]
sample2D f xs ys = [[f x y | y <- ys] | x <- xs]

sample2DM :: (Element a, Element b, Indexable (Vector a) a)
             => (a -> a -> b)
             -> Vector a
             -> Vector a
             -> Matrix b
sample2DM f vx vy = buildMatrix (dim vx) (dim vy) (\(i, j) -> f (vx ! i) (vy ! j))

square :: Num a => a -> a
square x = x * x

clamp :: Double -> Double
clamp x
  | x > 1     = 1
  | x < 0     = 0
  | otherwise = x

norm :: Floating a => a -> a -> a
norm x y = sqrt $ square x + square y
