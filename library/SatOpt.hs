{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Main module
module SatOpt (module SatOpt) where

import           SatOpt.FFT                    as SatOpt
import           SatOpt.Render                 as SatOpt
import           SatOpt.Utility                as SatOpt
-- GENERATE: import New.Module as SatOpt

import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL
import           Data.Complex
import           Data.Packed.Matrix            (mapMatrix)
import           Numeric.LinearAlgebra.HMatrix hiding (disp, matrix, size)
--import qualified Numeric.LinearAlgebra.HMatrix as HM

data Field2D a = FMat  (Matrix a)
               | FFT   (Matrix a)
               | FDisp (Matrix a)
               | FFunc (ℝ -> ℝ -> a)

data State = State { amplitude  :: Field2D ℂ
                   , dispersion :: Field2D ℂ
                   , sources    :: Field2D ℂ
                   , time       :: ℝ
                   , freq       :: ℝ
                   , concrete   :: Bool
                   }

constF :: Element a => a -> Field2D a
constF x = matField (FFunc (\_ _ -> x))

defState :: State
defState = State { amplitude  = constF (0 :+ 0)
                 , dispersion = constF (1 :+ 0)
                 , sources    = constF (0 :+ 0)
                 , time       = 0
                 , freq       = 1
                 , concrete   = False
                 }

fromField :: Element a => Field2D a -> Matrix a
fromField (FFunc f) = sample2DM f [0, 1 .. size - 1] [0, 1 .. size - 1]
fromField (FMat m)  = m
fromField (FFT m)   = m
fromField (FDisp m) = m

matField :: Element a => Field2D a -> Field2D a
matField (FFunc f) = FMat $ sample2DM f [0, 1 .. size - 1] [0, 1 .. size - 1]
matField x         = x

fftMat :: Field2D ℂ -> Matrix ℂ
fftMat (FFT m)  = m
fftMat (FMat m) = fft2dM m
fftMat f        = fftMat $ matField f

dispMat :: Field2D ℂ -> Matrix ℂ
dispMat (FDisp m) = m
dispMat (FMat m)  = recip $ square m + square im + square jm
dispMat f         = dispMat $ matField f

makeConcrete :: State -> State
makeConcrete st@(State { concrete = True }) = st
makeConcrete st@(State { dispersion = d, sources = s, time = t, freq = f })
  = st { amplitude = ampl, dispersion = FDisp disp, sources = FFT srcs, concrete = True }
  where
    disp = dispMat $ matField d
    srcs = fftMat $ matField s
    ampl = FMat $ timeShift f t $ ifft2dM $ srcs * disp

phaseShift :: Matrix ℝ -> Matrix ℂ -> Matrix ℂ
phaseShift p m = mags * zipMatrixWith (:+) (cos angles) (sin angles)
  where
    angles = p + (mapMatrix realPart m `arctan2` mapMatrix imagPart m)
    mags = m * conj m

timeShift :: ℝ -> ℝ -> Matrix ℂ -> Matrix ℂ
timeShift f t m = case matField $ constF (tau * f * t) of
                   FMat p -> phaseShift p m
                   _      -> error "matField output invalid"

size :: Num a => a
size = 64

tau :: ℝ
tau = 2 * pi

rootsOfUnity :: Int -> [ℂ]
rootsOfUnity i = take i $ iterate (rotate (tau / fromIntegral i)) (1 :+ 0)

fromC :: ℂ -> (ℝ, ℝ)
fromC (a :+ b) = (a, b)

toC ::(ℝ, ℝ) -> ℂ
toC (a, b) = a :+ b

scaleC :: ℝ -> ℂ -> ℂ
scaleC r c = (r :+ 0) * c

pointsOnCircle :: Int -> ℝ -> [(ℝ, ℝ)]
pointsOnCircle i r = map (fromC . scaleC r) $ rootsOfUnity i

-- | Take a position with respect to the center and move its origin to the top right
center :: Floating a => a -> a
center x = x - (size / 2)

cnorm :: Floating a => a -> a -> a
cnorm x y = norm (center x) (center y)

window :: ℝ -> ℝ -> ℝ
window x y = (+ 0.5) $ clamp $ subtract 0.5 $ (* (size/8)) $ recip $ cnorm x y

pointSources :: [(ℝ, ℝ, ℂ)] -> ℝ -> ℝ -> ℂ
pointSources ps x y = sum $ map (\(cx, cy, s) -> s * delta cx cy x y) ps

circleOfSources :: ℝ -> [ℂ] -> ℝ -> ℝ -> ℂ
circleOfSources rad cs = pointSources $ zipCs $ pointsOnCircle (length cs) rad
  where
    zipCs = map (\(z, (x, y)) -> (x, y, z)) . zip cs

testFunc :: ℝ -> ℝ -> ℂ
testFunc = circleOfSources 0.1 [ pol 1 0
                               , pol 1 45
                               , pol 1 45
                               , pol 1 0
                               ]
  where
    pol m p = mkPolar m ((pi/180)*p)
-- testFunc = pointSources [ (0.48, 0.48, 0.5 :+ 0)
--                         , (0.52, 0.52, (-0.5) :+ 0)
--                         ]

wavenumF :: ℝ -> ℝ -> ℂ
wavenumF _ _ = 1 :+ 0

wavenum :: Matrix ℂ
wavenum = sample2DM wavenumF [0, 1 .. size - 1] [0, 1 .. size - 1]
--  mapMatrix (const (1 :+ 0)) im

im, jm :: Matrix ℂ
im = sample2DM (\x _ -> x :+ 0) [0, 1 .. size - 1] [0, 1 .. size - 1]
jm = sample2DM (\_ y -> y :+ 0) [0, 1 .. size - 1] [0, 1 .. size - 1]

func1 :: Matrix ℂ -> Matrix ℂ
func1 x = ifft2dM $ fft2dM x / ((wavenum * wavenum) + (im * im) + (jm * jm))

eps :: ℝ
eps = 0.01

delta :: ℝ -> ℝ -> ℝ -> ℝ -> ℂ
delta cx cy x y
  | norm ((center x / size) - cx) ((center y / size) - cy) < eps    = 1
  | otherwise                                                       = 0

matrix :: Matrix ℂ
matrix = sample2DM testFunc [0, 1 .. size - 1] [0, 1 .. size - 1]

rendCmp :: ℂ -> (ℝ, ℝ, ℝ)
rendCmp c = uncurryRGB (\x y z -> (x, y, z)) $ hsl a 0.25 m
  where
    m = magnitude c
    a = (* (180 / pi)) $ (+ pi) $ phase c

test1, test2, test3 :: Matrix ℂ
test1 = matrix
test2 = ifft2dM $ fft2dM matrix
test3 = func1 matrix

indx :: (Indexable m v, Indexable v e) => Int -> Int -> m -> e
indx i j m = (m ! i) ! j

rotate :: ℝ -> ℂ -> ℂ
rotate r = uncurry mkPolar . (\(m, p) -> (m, p + r)) . polar

visual :: (ℝ, ℝ, ℝ, ℝ) -> ℂ -> ℂ
visual (xa, xb, ya, yb) (polar -> (m, p))
  = mkPolar (((yb - ya) * (m - xa)/(xb - xa)) + ya) p

visualM :: Matrix ℂ -> Matrix ℂ
visualM cs = mapMatrix (visual (small, big, 0, 1)) cs
  where
    mags  = map magnitude $ toList $ flatten cs
    small = minimum mags
    big   = maximum mags

updateState :: State -> State
updateState s@State { time = t } = s { time = t + 0.1, concrete = False }

renderState :: State -> [[RGBTrip]]
renderState = map (map rendCmp) . toLists . visualM . ampField
  where
  ampField = fromField . concAmp
  concAmp = amplitude . makeConcrete

config :: Conf State
config = Conf { keyBinds = const id
              , state    = defState { sources = FMat test3, freq = 1 }
              , evolve   = updateState
              , render   = renderState
              , canvas   = (size, size)
              }

-- | TODO
main :: IO ()
main = runDisp config
