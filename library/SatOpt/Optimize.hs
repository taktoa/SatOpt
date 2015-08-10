{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE ViewPatterns   #-}

-- | TODO
module SatOpt.Optimize where
import           Data.Ratio
import           Debug.Trace

-- | Standard gravitational parameter for Earth
μe :: Floating f => f
μe = 398600.441 * (1000^3)

-- | Radius of Earth
re :: Num n => n
re = 6371000

-- | Number of people on Earth
nump :: Num n => n
nump = 7000000000

data OptConf = OptConf { peru :: Double
                         -- ^ Fraction of people on Earth who are users
                       , numl :: Int
                         -- ^ Number of launches
                       }

-- | The number of satellites
nums :: Num n => OptConf -> n
nums OptConf { numl } = fromIntegral $ numl * 10

-- | The number of users
numu :: Num n => OptConf -> n
numu c@(nums -> ns) = fromIntegral $ round $ (nump * (peru c) / ns)

log10 :: Floating a => a -> a
log10 = logBase 10


-- | $#_s \cdot (1 - \frac{R_E}{\sqrt{R_E^2 + d^2 \tan^2{\theta_s}}}) = 2$
θs_ :: OptConf -> Double -> Double
θs_ c a = atan $ sqrt $ (sq re - sq (re / (1 - (2 / (nums c))))) / sq a
  where
    sq x = x * x


alt_ :: OptConf -> Double -> Double
alt_ c th = sqrt $ fromRational rhs
  where
    nu :: Double
    nu = numu c
    costh = (1 - cos th) ** 4
    rhs = (10 ^^ (round $ lhs - 25.3)) / (toRational $ nu * costh)
    !lhs = big * ((approxRational 0.0001 $ log10 nu) - 13)
    big :: Rational
    !big = 2 ^ exp
    exp :: Integral i => i
    exp = round $ toRational (0.15 * nu)


approximate :: (Double -> b)
            -> (b -> Double)
            -> Double
            -> Double
approximate f g x = if applied `approx` x then trace (show x) x else trace (show applied) applied
  where
    applied = g $ f x
    eps = 0.01
    approx a b = abs (a - b) < eps

θs :: OptConf -> Double
θs c = approximate (alt_ c) (θs_ c) 0.01

alt :: OptConf -> Double
alt c = alt_ c (θs c)

-- | Main function for optimization module
optimize :: IO ()
optimize = return ()

testC :: OptConf
testC = OptConf { peru = 0.00001, numl = 20 }
