-- | TODO
module SatOpt.Utility where


sample2D :: (a -> a -> b) -> [a] -> [a] -> [[b]]
sample2D f xs ys = [[f x y | y <- ys] | x <- xs]

square :: Num a => a -> a
square x = x * x

clamp :: Double -> Double
clamp x
  | x > 1     = 1
  | x < 0     = 0
  | otherwise = x

norm :: Floating a => a -> a -> a
norm x y = sqrt $ square x + square y
