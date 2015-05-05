{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE ViewPatterns   #-}

-- | TODO
module SatOpt.Optimize where
import           Data.Ratio
import           Debug.Trace
import           Numeric.Units.Dimensional.TF            (Dimensionless, Length,
                                                          Unit, one, (*~), (/~))
import           Numeric.Units.Dimensional.TF.NonSI
import           Numeric.Units.Dimensional.TF.Prelude    ()
import qualified Numeric.Units.Dimensional.TF.Prelude    as D
import           Numeric.Units.Dimensional.TF.Quantities
import           Numeric.Units.Dimensional.TF.SIUnits

(&/) = (D./)
(&*) = (D.*)
(&+) = (D.+)
(&-) = (D.-)
--(&^) = (D.^)
dsqrt = (D.sqrt)

km3s2 :: Unit DGravitationalParameter Double
km3s2 = cubic (kilo meter) &/ (second &* second)

-- | Standard gravitational parameter for Earth
μe :: GravitationalParameter Double
μe = 398600.441 *~ km3s2

-- | Radius of Earth
re :: Length Double
re = 6371 *~ kilo meter

-- | Number of people on Earth
nump :: Num n => Dimensionless n
nump = 7000000000 *~ one

data OptConf = OptConf { peru :: Double
                         -- ^ Fraction of people on Earth who are users
                       , numl :: Int
                         -- ^ Number of launches
                       }

-- | A dimensionless number 10
_10 :: Num a => Dimensionless a
_10 = 10 *~ one

-- | The number of satellites
nums :: Num n => OptConf -> n
nums OptConf { numl } = fromIntegral $ (/~ one) $ (* _10) $ (numl *~ one)

-- | The number of users
numu :: Num n => OptConf -> n
numu c@(nums -> ns) = fromIntegral $ round $ ((nump * (peru c) / ns) /~ one)

log10 :: Floating a => a -> a
log10 = logBase 10


-- | $#_s \cdot (1 - \frac{R_E}{\sqrt{R_E^2 + d^2 \tan^2{\theta_s}}}) = 2$
θs_ :: OptConf -> Length Double -> Dimensionless Double
θs_ c a = atan $ sqrt $ (sq re - sq (re / (_1 - (_2 / (nums c))))) / sq a
  where
    sq x = x * x


alt_ :: OptConf -> Dimensionless Double -> Length Double
alt_ c@(numu -> nu) th = sqrt rhs &* (1 *~ meter)
  where
    costh = (_1 - cos th) ** _4
    rhs = (_10 ** (lhs - (25.3 *~ one))) / (nu * costh)
    lhs = (trace (show big) (big *~ one)) * ((log10 nu) - (13 *~ one))
    big :: Rational
    big = (2 Prelude.^ exp)
    exp :: Rational
    exp = toRational (15 * (nu /~ one))


approximate :: (Dimensionless Double -> b)
            -> (b -> Dimensionless Double)
            -> Dimensionless Double
            -> Dimensionless Double
approximate f g x = if applied `approx` x then trace (show x) x else trace (show applied) applied
  where
    applied = g $ f x
    eps = 0.01 *~ one
    approx a b = abs (a - b) < eps

θs :: OptConf -> Dimensionless Double
θs c = approximate (alt_ c) (θs_ c) (0.01 *~ one)

alt :: OptConf -> Length Double
alt c = alt_ c (θs c)

-- | Main function for optimization module
optimize :: IO ()
optimize = return ()

testC :: OptConf
testC = OptConf { peru = 0.00001 *~ one, numl = 20 *~ one }
