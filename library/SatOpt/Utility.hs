{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | TODO
module SatOpt.Utility where

import           Data.Packed.Matrix            (liftMatrix2)
import           Data.Packed.Vector            (zipVectorWith)
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

--class Conv a b | a -> b where
class Conv a b where
  convert :: a -> b

instance Conv Double  Int where convert = round
instance Conv Float   Int where convert = round
instance Conv Int     Int where convert = id
instance Conv Integer Int where convert = fromIntegral

sample2DM :: (Container Vector a, Container Matrix b, Indexable (Vector a) a, Conv b Int)
             => (a -> a -> b)
             -> Vector a
             -> Vector a
             -> Matrix b
sample2DM f vx vy = build (size vx, size vy) (\i j -> f (vx ! convert i) (vy ! convert j))
--  where
--    rows = fromRows $ replicate (size vy) vx
--    columns = fromColumns $ replicate (length vx) vy
-- fromColumns $ replicate (length testx) (fromList testy)
-- fromRows $ replicate (length testy) (fromList testx)
-- let zipMatrix f = liftMatrix2 (zipVectorWith f)

square :: Num a => a -> a
square x = x * x

clamp :: Double -> Double
clamp x
  | x > 1     = 1
  | x < 0     = 0
  | otherwise = x

norm :: Floating a => a -> a -> a
norm x y = sqrt $ square x + square y
