module Main where

data Vec3 a = Vec3 a a a
            deriving (Read, Show)

instance Num a => Num (Vec3 a) where
  (Vec3 a b c) + (Vec3 x y z) = Vec3 (a + x) (b + y) (c + z)
  (Vec3 a b c) * (Vec3 x y z) = Vec3 (a * x) (b * y) (c * z)
  abs (Vec3 a b c) = Vec3 (abs a) (abs b) (abs c)
  fromInteger = fromScalar . fromInteger
  negate (Vec3 a b c) = Vec3 (negate a) (negate b) (negate c)
  signum (Vec3 a b c) = Vec3 (signum a) (signum b) (signum c)

inf :: Fractional a => a
inf = 1 / 0

ninf :: Fractional a => a
ninf = -1 / 0

fromScalar :: Num a => a -> Vec3 a
fromScalar x = Vec3 x x x

scale :: Num a => a -> Vec3 a -> Vec3 a
scale r v = v * fromScalar r

ihat :: Num a => Vec3 a
ihat = Vec3 1 0 0

jhat :: Num a => Vec3 a
jhat = Vec3 0 1 0

khat :: Num a => Vec3 a
khat = Vec3 0 0 1

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 a b c) (Vec3 x y z) = Vec3 p q r
  where
    (p, q, r) = (b*z - c*y, c*x - a*z, a*y - b*x)

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 a b c) (Vec3 x y z) = (a * x) + (b * y) + (c * z)

data OrbitS = OrbitS { osrvec :: Vec3 Double
                     , osvvec :: Vec3 Double
                     } deriving (Read, Show)

magnitude :: Floating a => Vec3 a -> a
magnitude (Vec3 a b c) = sqrt ((a*a) + (b*b) + (c*c))

recipmag :: Floating a => Vec3 a -> a
recipmag = recip . magnitude

recipmags :: Floating a => [Vec3 a] -> a
recipmags = product . map recipmag

normalize :: Floating a => Vec3 a -> Vec3 a
normalize v = recipmag v `scale` v

oshvec :: OrbitS -> Vec3 Double
oshvec (OrbitS rv vv) = rv `cross` vv

osrmag :: OrbitS -> Double
osrmag = magnitude . osrvec

osvmag :: OrbitS -> Double
osvmag = magnitude . osvvec

oshmag :: OrbitS -> Double
oshmag = magnitude . oshvec

osnvec :: OrbitS -> Vec3 Double
osnvec o = khat `cross` oshvec o

osrdotv :: OrbitS -> Double
osrdotv (OrbitS rv vv) = rv `dot` vv

mu :: Fractional a => a
mu = 398600.4418

osevec :: OrbitS -> Vec3 Double
osevec o@(OrbitS rv vv) = scale (1 / mu) numerator
  where
    numerator = (((vv `dot` vv) - (mu / rm)) `scale` rv) - (rdotv `scale` vv)
    rm = magnitude rv
    rdotv = osrdotv o

osemag :: OrbitS -> Double
osemag = magnitude . osevec

oseps :: OrbitS -> Double
oseps (OrbitS rv vv) = ((vv `dot` vv) / 2) - (mu / magnitude rv)

osa :: OrbitS -> Double
osa o = - mu / (2 * oseps o)

ose :: OrbitS -> Double
ose = osemag

osi :: OrbitS -> Double
osi o = acos (recipmag hv * (hv `dot` khat))
  where
    hv = oshvec o

osO :: OrbitS -> Double
osO o
  | nj < 0    = 2*pi - ounadj
  | otherwise = ounadj
  where
    ounadj = acos (recipmag nv * ni)
    nv@(Vec3 ni nj _) = osnvec o

osw :: OrbitS -> Double
osw o
  | ek < 0    = 2*pi - wunadj
  | otherwise = wunadj
  where
    wunadj = acos ((nv `dot` ev) * recipmags [nv, ev])
    nv = osnvec o
    ev@(Vec3 _ _ ek) = osevec o

osf :: OrbitS -> Double
osf o@(OrbitS rv _)
  | osrdotv o < 0 = 2*pi - funadj
  | otherwise     = funadj
  where
    funadj = acos (rv `dot` ev * recipmags [ev, rv])
    ev = osevec o

data OrbitC = OrbitC { oca :: Double -- semi-major axis
                     , oce :: Double -- eccentricity
                     , oci :: Double -- inclination
                     , ocO :: Double -- right ascension
                     , ocw :: Double -- argument of perigee
                     , ocf :: Double -- true anomaly
                     } deriving (Read, Show)

data OrbitPlane = Equatorial | Inclined | Polar
                deriving (Read, Show, Enum, Ord, Eq)

data OrbitType = Circular | Elliptical | Hyperbolic | Parabolic
               deriving (Read, Show)

type OrbitShape = (OrbitPlane, OrbitType)

ocrvec :: OrbitC -> Vec3 Double
ocrvec (OrbitC { oca = a, oce = e, oci = i, ocO = o, ocw = w, ocf = f })
  = scale rm (Vec3 rdx rdy rdz)
  where
    rm = a * (1 - e*e) / (1 + e * cos f)
    theta = w + f
    (cosi, coso, costh) = (cos i, cos o, cos theta)
    (sini, sino, sinth) = (sin i, sin o, sin theta)
    rdx = (coso * costh) - (sino * sinth * cosi)
    rdy = (sino * costh) + (coso * sinth * cosi)
    rdz = sinth * sini

ocvdir :: OrbitC -> Vec3 Double
ocvdir (OrbitC {oce = e, oci = i, ocO = o, ocw = w, ocf = f })
  = normalize $ Vec3 vdx vdy vdz
  where
    theta = w + f
    (cosi, coso, costh, ecosw) = (cos i, cos o, cos theta, e * cos w)
    (sini, sino, sinth, esinw) = (sin i, sin o, sin theta, e * sin w)
    vdx = (coso * (sinth + esinw)) + (sino * (costh + ecosw) * cosi)
    vdy = (sino * (sinth + esinw)) - (coso * (costh + ecosw) * cosi)
    vdz = - sini * (costh + ecosw)

ocvmag :: OrbitC -> Double
ocvmag o@(OrbitC { oca = a }) =
  - sqrt (mu * ((2 / rm) - (1 / a)))
  where
    rm = magnitude $ ocrvec o

ocvvec :: OrbitC -> Vec3 Double
ocvvec o = scale (ocvmag o) (ocvdir o)

delta :: Floating a => a
delta = 0.01

(~=) :: (Floating a, Ord a) => a -> a -> Bool
x ~= y = abs (x - y) < delta

osType :: OrbitS -> OrbitType
osType = ocType . convSC

ocType :: OrbitC -> OrbitType
ocType (OrbitC { oce = ecc })
  | ecc ~= 0  = Circular
  | ecc ~= 1  = Parabolic
  | ecc > 1   = Hyperbolic
  | otherwise = Elliptical

osPlane :: OrbitS -> OrbitPlane
osPlane = ocPlane . convSC

ocPlane :: OrbitC -> OrbitPlane
ocPlane (OrbitC { oci = i })
  | i ~= 0        = Equatorial
  | i ~= (pi / 2) = Polar
  | otherwise     = Inclined

osShape :: OrbitS -> OrbitShape
osShape = ocShape . convSC

ocShape :: OrbitC -> OrbitShape
ocShape o = (ocPlane o, ocType o)

convSC :: OrbitS -> OrbitC
convSC o = OrbitC { oca = osa o
                  , oce = ose o
                  , oci = osi o
                  , ocO = osO o
                  , ocw = osw o
                  , ocf = osf o
                  }

convCS :: OrbitC -> OrbitS
convCS o = OrbitS { osrvec = ocrvec o
                  , osvvec = ocvvec o
                  }

hw1A :: OrbitS
hw1A = OrbitS hw1Ar hw1Av
  where
    hw1Ar = Vec3 4823.6270 1823.1491 4551.65289
    hw1Av = Vec3 (-5.426735) 2.014869 4.943952

hw1B :: OrbitS
hw1B = OrbitS hw1Br hw1Bv
  where
    hw1Br = Vec3 (-9937.277645) 15004.226326 0.000663
    hw1Bv = Vec3 (-2.823536) (-3.034859) 0.000001

hw1C :: OrbitS
hw1C = OrbitS hw1Cr hw1Cv
  where
    hw1Cr = Vec3 (-3543.73651) 17727.106997 (-4177.470290)
    hw1Cv = Vec3 (-4.064771) (-1.478711) 0.876872

hw2A :: OrbitC
hw2A = OrbitC { oca = 9000
              , oce = 0.05
              , oci = radians 45
              , ocw = radians 137
              , ocO = radians 2.7
              , ocf = radians 97.4
              }

hw2B :: OrbitC
hw2B = OrbitC { oca = 7300
              , oce = 0.1
              , oci = radians 100
              , ocw = radians 44
              , ocO = radians 160
              , ocf = radians 305
              }

hw2C :: OrbitC
hw2C = OrbitC { oca = a
              , oce = rp / a
              , oci = radians 0
              , ocw = radians 78
              , ocO = radians 0
              , ocf = radians 147
              }
  where
    a = (rp + ra) / 2
    rp = 11230
    ra = 36010

vecPretty :: Show a => Vec3 a -> String
vecPretty (Vec3 x y z) = show (x, y, z)

osPretty :: OrbitS -> String
osPretty (OrbitS r v) = "Position: " ++ offset ++ vecPretty r ++ "\n" ++
                        "Velocity: " ++ offset ++ vecPretty v
  where
    offset = replicate 9 ' '

radians :: Double -> Double
radians x = pi * x / 180

degrees :: Double -> Double
degrees x = 180 * x / pi

ocPretty :: OrbitC -> String
ocPretty (OrbitC a e i o w f) = "Semi-major axis:  " ++ oshow a  ++ "\n" ++
                                "Eccentricity:     " ++ oshow e  ++ "\n" ++
                                "Inclination:      " ++ oshow i' ++ "\n" ++
                                "RAAN:             " ++ oshow o' ++ "\n" ++
                                "Arg. of perigee:  " ++ oshow w' ++ "\n" ++
                                "True anomaly:     " ++ oshow f'
  where
    oshow x = if x < 0 then show x else ' ' : show x
    [i', o', w', f'] = map degrees [i, o, w, f]

shapePretty :: OrbitShape -> String
shapePretty (p, t) = "Shape: " ++ offset ++ show p ++ " " ++ tt
  where
    offset = replicate 12 ' '
    tt = case t of
      Circular   -> "circle"
      Elliptical -> "ellipse"
      Hyperbolic -> "hyperbola"
      Parabolic  -> "parabola"

qString :: [String] -> [String] -> [String]
qString units = pad . zip units
  where
    mls f = map (f . length . snd)
    padL = maximum . mls id
    padding xs = mls (flip replicate ' ' . (1 -) . subtract (padL xs)) xs
    pad xs = map (\(p, (a, b)) -> b ++ p ++ a ++ "\n") $ zip (padding xs) xs

q1Print :: OrbitS -> IO ()
q1Print hw = putStrLn $ concat $ qString units $
              lines (osPretty hw)              ++
              lines (ocPretty $ convSC hw)     ++
              lines (shapePretty $ osShape hw)
  where
    units = [ "km", "km/s", "km", "", "degrees", "degrees", "degrees", "degrees", "" ]

q2Print :: OrbitC -> IO ()
q2Print hw = putStrLn $ concat $ qString units $
             lines (osPretty $ convCS hw)     ++
             lines (ocPretty hw)              ++
             lines (shapePretty $ ocShape hw)
  where
    units = [ "km", "km/s", "km", "", "degrees", "degrees", "degrees", "degrees", "" ]

main :: IO ()
main = do
  putStrLn "Answer for 1a:"
  q1Print hw1A
  putStrLn "Answer for 1b:"
  q1Print hw1B
  putStrLn "Answer for 1c:"
  q1Print hw1C
  putStrLn "Answer for 2a:"
  q2Print hw2A
  putStrLn "Answer for 2b:"
  q2Print hw2B
  putStrLn "Answer for 2c:"
  q2Print hw2C
