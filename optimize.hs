{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE ViewPatterns   #-}

module Main where
import           Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude                              ()

km3s2 :: Fractional a => Unit DGravitationalParameter a
km3s2 = cubic (kilo meter) / (second * second)

-- | Standard gravitational parameter for Earth
μe :: GravitationalParameter Double
μe = 398600.441 *~ km3s2

-- | Radius of Earth
re :: Length Double
re = 6371 *~ kilo meter

-- | Number of people on Earth
nump :: Dimensionless Int
nump = 7000000000 *~ one

data Conf = Conf { d    :: Length Double        -- ^ Satellite altitude
                 , peru :: Dimensionless Double -- ^ Fraction of people on Earth who are users
                 , numl :: Dimensionless Int    -- ^ Number of launches
                 }

_10 :: Num a => Dimensionless a
_10 = 10 *~ one

nums :: Conf -> Dimensionless Int
nums Conf { numl } = numl * _10

numu :: Conf -> Dimensionless Int
numu c@(nums -> ns) = round <$> ((fromIntegral <$> nump) * (peru c) / (fromIntegral <$> ns))


-- | $#_s \cdot (1 - \frac{R_E}{\sqrt{R_E^2 + d^2 \tan^2{\theta_s}}}) = 2$
θs :: Conf -> Dimensionless Double
θs c@(Conf { d }) = atan $ sqrt $ ((sq re) - (sq (re / (_1 - (_2 / ns))))) / (sq d)
  where
    sq x = x * x
    ns = fromIntegral <$> nums c

main :: IO ()
main = return ()
