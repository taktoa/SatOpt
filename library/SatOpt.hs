{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Main module
module SatOpt (main) where
--module SatOpt (module SatOpt) where

import           SatOpt.FFT                    as SatOpt
import           SatOpt.Optimize               as SatOpt
import           SatOpt.Render                 as SatOpt
import           SatOpt.Utility                as SatOpt
-- GENERATE: import New.Module as SatOpt

import           Data.Colour                   (blend)
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL
import           Data.Complex
import           Data.List
import           Data.Packed.Matrix            (mapMatrix)
import           Debug.Trace                   (trace)
import           Numeric.LinearAlgebra.HMatrix hiding (disp, matrix, size)
import qualified Numeric.LinearAlgebra.HMatrix as HM

data Field2D a = FMat  (Matrix a)
               | FFT   (Matrix a)
               | FDisp (Matrix a)
               | FFunc (ℝ -> ℝ -> a)

data HSource = HPointSources  [(ℝ, ℝ, ℂ)]
             | HCircleSources ℝ [ℂ]
             | HSum [HSource]

hsrcToField :: HSource -> Field2D ℂ
hsrcToField (HPointSources ps)   = FFunc $ pointSources ps
hsrcToField (HCircleSources r p) = FFunc $ circleOfSources r p
hsrcToField (HSum [h])           = hsrcToField h
hsrcToField (HSum (h:hs))        = fieldSum (hsrcToField h) (hsrcToField (HSum hs))

fieldSum :: Field2D ℂ -> Field2D ℂ -> Field2D ℂ
fieldSum (FMat m1)  (FMat m2)  = FMat (m1 + m2)
fieldSum (FFunc f1) (FFunc f2) = FFunc (\x y -> f1 x y + f2 x y)
fieldSum (FMat m1)  (FFunc f2) = fieldSum (FFunc f2) (FMat m1)
fieldSum (FFunc f1) (FMat m2)  = fieldSum (matField (FFunc f1)) (FMat m2)

data HDisp = HConstDisp ℂ
           | HFuncDisp (ℝ -> ℝ -> ℂ)

hdspToField :: HDisp -> Field2D ℂ
hdspToField (HConstDisp p) = constF p
hdspToField (HFuncDisp f) = FFunc f

data HelmConf = HelmConf { helmsrc :: HSource
                         , helmdsp :: HDisp
                         }

data State a = State { amplitude  :: Field2D ℂ
                     , dispersion :: Field2D ℂ
                     , sources    :: Field2D ℂ
                     , time       :: ℝ
                     , freq       :: ℝ
                     , conf       :: a
                     , configure  :: State a -> State a
                     , concrete   :: Bool
                     }

constF :: Element a => a -> Field2D a
constF x = matField (FFunc (\_ _ -> x))

defState :: State a
defState = State { amplitude  = constF (0 :+ 0)
                 , dispersion = constF (1 :+ 0.1)
                 , sources    = constF (0 :+ 0)
                 , time       = 0
                 , freq       = 1
                 , conf       = undefined
                 , configure  = const defState
                 , concrete   = False
                 }

fromFieldC :: Field2D ℂ -> Matrix ℂ
fromFieldC (FFunc f) = sample2DM f [0, 1 .. size - 1] [0, 1 .. size - 1]
fromFieldC (FMat m)  = m
fromFieldC (FFT m)   = ifft2dM m
fromFieldC (FDisp m) = sqrt $ (recip m) - (square normm)

matField :: Element a => Field2D a -> Field2D a
matField (FFunc f) = FMat $ sample2DM f [0, 1 .. size - 1] [0, 1 .. size - 1]
matField x         = x

fftMat :: Field2D ℂ -> Matrix ℂ
fftMat (FFT m)  = m
fftMat (FMat m) = fft2dM m
fftMat f        = fftMat $ matField f

dispMat :: Field2D ℂ -> Matrix ℂ
dispMat (FDisp m) = m
dispMat (FMat m)  = recip $ square m + square normm
dispMat f         = dispMat $ matField f

makeConcrete :: (State a) -> (State a)
makeConcrete st@(State { concrete = True }) = st
makeConcrete (st@(State { dispersion = d, sources = s, time = t, freq = f }))
  = st { amplitude = ampl
       , dispersion = FDisp disp
       , sources = FFT srcs
       , concrete = True }
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
size = 512

tau :: ℝ
tau = 2 * pi

rootsOfUnity :: Int -> [ℂ]
rootsOfUnity i = take i $ iterate (rotate (tau / fromIntegral i)) (1 :+ 0)

pointsOnCircle :: Int -> ℝ -> [(ℝ, ℝ)]
pointsOnCircle i r = map (fromC . scaleC r) $ rootsOfUnity i
  where
    fromC (a :+ b) = (a, b)
    scaleC s c = (s :+ 0) * c

-- | Take a position with respect to the center and move its origin to the top right
center :: Floating a => a -> a
center x = x - (size / 2)

pointSources :: [(ℝ, ℝ, ℂ)] -> ℝ -> ℝ -> ℂ
pointSources ps x y = sum $ map (\(cx, cy, s) -> s * delta cx cy x y) ps

circleOfSources :: ℝ -> [ℂ] -> ℝ -> ℝ -> ℂ
circleOfSources rad cs = pointSources $ zipCs $ pointsOnCircle (length cs) rad
  where
    zipCs = map (\(z, (x, y)) -> (x, y, z)) . zip cs


--const $ FFunc testFunc

-- testFunc :: HelmConf -> Field2D ℂ
-- testFunc (HelmConf (HPointSources ps) _)   = FFunc $ pointSources ps
-- testFunc (HelmConf (HCircleSources r p) _) = FFunc $ circleOfSources r p
  -- where
  --   pol m p = mkPolar m ((pi/180)*p)
-- testFunc = pointSources [ (0.48, 0.48, 0.5 :+ 0)
--                         , (0.52, 0.52, (-0.5) :+ 0)
--                         ]

-- 0.1 [ pol 1 0
--                                , pol 1 60
--                                , pol 1 120
--                                , pol 1 120
--                                , pol 1 60
--                                , pol 1 0
--                                ]

normm :: Matrix ℂ
--im, jm :: Matrix ℂ
--im = sample2DM (\x _ -> x :+ 0) [0, 1 .. size - 1] [0, 1 .. size - 1]
--jm = sample2DM (\_ y -> y :+ 0) [0, 1 .. size - 1] [0, 1 .. size - 1]
normm = sample2DM (\x y -> norm x y :+ 0) [0, 1 .. size - 1] [0, 1 .. size - 1]

eps :: ℝ
eps = 5

delta :: ℝ -> ℝ -> ℝ -> ℝ -> ℂ
delta ((*size) -> cx) ((*size) -> cy) ((*size) -> x) ((*size) -> y)
  | norm (center x - cx) (center y - cy) < size * eps    = 1
  | otherwise                                                       = 0

rendCmp_ :: ℂ -> (ℝ, ℝ, ℝ)
rendCmp_ (polar -> (m, p)) = uncurryRGB (\x y z -> (x, y, z)) $ hsl a 1 m
  where
--    a = if p < 0 then 90 else 0
    a = (p + pi) * (180 / pi)

rendCmp :: Matrix ℂ -> (Matrix ℝ, Matrix ℝ, Matrix ℝ)
rendCmp = matrify . unzip3 . map (unzip3 . map rendCmp_) . toLists
  where
    matrify (x, y, z) = (fromLists x, fromLists y, fromLists z)

rotate :: ℝ -> ℂ -> ℂ
rotate r = uncurry mkPolar . (\(m, p) -> (m, p + r)) . polar

visual :: (ℝ, ℝ, ℝ, ℝ) -> ℂ -> ℂ
visual (xa, xb, ya, yb) (polar -> (m, p))
  = mkPolar (((yb - ya) * (m - xa)/(xb - xa)) + ya) p

visualM :: Matrix ℂ -> Matrix ℂ
visualM cs@(fromComplex -> (r, i)) = mapMatrix (visual (small, big, 0, 1)) cs
  where
    !mags  = flatten $ sqrt $ (r * r) + (i * i)
    !small = minElement mags
    !big   = maxElement mags

updateState :: State a -> State a
--updateState = id
updateState s@State { time = t } = s { time = t + 0.01, concrete = False }

data RenderMode = RendAmp | RendSrc | RendDsp | RendAmpMag | RendAmpPhs

renderState :: RenderMode -> State a -> RGBTrips
renderState r = case r of
  RendAmp    -> rend . concAmp
  RendAmpMag -> rend . concAmpMag
  RendAmpPhs -> rend . concAmpPhs
  RendSrc    -> rend . concSrc
  RendDsp    -> rend . concDsp
  where
  wrap f = fromFieldC . f . makeConcrete
  rend =  rendCmp . visualM
  concAmp    = wrap amplitude
  concAmpMag = mag . fromComplex . concAmp
  concAmpPhs = phs . fromComplex . concAmp
  concSrc    = wrap sources
  concDsp    = wrap dispersion
  mag (x, y) = toComplex $ ((x*x) + (y*y), y - y)
  phs (x, y) = toComplex $ ((y `arctan2` x), y - y)

changeRenderMode :: RenderMode -> Conf (State a) -> Conf (State a)
changeRenderMode r c = c { render = renderState r }

getConf :: Conf (State HelmConf) -> HelmConf
getConf (Conf { state = (State { conf = cn })}) = cn

changeConf :: (HelmConf -> HelmConf) -> Conf (State HelmConf) -> Conf (State HelmConf)
changeConf h c@(Conf { state = s@(State { configure = f, conf = cn })})
  = c { state = configure s' s' }
  where
    s' = s { conf = h cn }

relPhase (polar -> (_, p1)) (polar -> (_, p2)) = abs $ p1 - p2

--getPhase c@(HelmConf { helmsrc = HCircleSources _ [c1, c2] }) = Just $ relPhase c1 c2
--getPhase _ = Nothing
getPhase c@(HelmConf { helmsrc = HPointSources [_, (x, _, _)] }) = Just x
getPhase _ = Nothing

changePhase :: Double -> Conf (State HelmConf) -> Conf (State HelmConf)
changePhase p = changeConf shift
  where
    shift c@(getPhase -> m) = case m of
                               Just ph -> c { helmsrc = testSource $ ph + p }
                               Nothing -> c

keyState :: KMState -> Conf (State HelmConf) -> IO (Conf (State HelmConf))
keyState ""  c = return c
keyState "d" c = return $ changeRenderMode RendDsp c
keyState "s" c = return $ changeRenderMode RendSrc c
keyState "a" c = return $ changeRenderMode RendAmp c
keyState "m" c = return $ changeRenderMode RendAmpMag c
keyState "p" c = return $ changeRenderMode RendAmpPhs c
keyState "w" c = do
  let p = changePhase 0.5 c
  putStrLn $ "Shifted by 0.5 to " ++ show (getPhase $ getConf c)
  return p
keyState "q" c = do
  let p = changePhase (-0.5) c
  putStrLn $ "Shifted by -0.5 to " ++ show (getPhase $ getConf c)
  return p
keyState x   c = return $ trace x c

pol m p = mkPolar m (p * pi / 180)

testSource :: Double -> HSource
testSource p = HPointSources [ (size/2, size/2, (-1) :+ 0)
                             , (p, p, 1 :+ 0)
                             ]
-- testSource p = HSum [ HCircleSources 0.2  [ pol 1 0
--                                           , pol 1 0
--                                           , pol 1 0
--                                           , pol 1 0
--                                           ]
--                     , HCircleSources 0.15 [ pol 1 0
--                                           , pol 1 90
--                                           , pol 1 180
--                                           , pol 1 270
--                                           ]
--                     ]

dspfun :: ℝ -> ℝ -> ℂ
dspfun x y
  | outbox (center x) (center y) = 1.2 :+ 0
  | otherwise                    = 1 :+ 0
  where
    test = 1
    outbox a b
      | a < (-32) = True
      | a > 32    = True
      | b < (-32) = True
      | b > 32    = True
      | otherwise = False

testConf :: Double -> HelmConf
testConf p = HelmConf { helmsrc = testSource p
                      , helmdsp = HFuncDisp dspfun
                      }

-- testConf = HelmConf { helmsrc = HCircleSources 0.1 [ pol 1 0
--                                                    , pol 1 0
--                                                    , pol 1 0
--                                                    , pol 1 0
--                                                    , pol 1 0
--                                                    , pol 1 0 ]
--                     , helmdsp = HConstDisp 0.001
--                     }
--   where
--     pol m p = mkPolar m (p * pi / 180)

testConfigure st@(State { conf = HelmConf { helmdsp = d, helmsrc = s }})
  = st { sources = hsrcToField s
       , dispersion = hdspToField d
       , concrete = False
       }

initState :: State HelmConf
initState = defState { freq = 3
                     , conf = testConf 0
                     , configure = testConfigure
                     }

config :: Conf (State HelmConf)
config = changeConf id
         Conf { keyBinds = keyState
              , state    = initState
              , evolve   = updateState
              , render   = renderState RendAmp
              , canvas   = (size, size)
              }

-- | TODO
main :: IO ()
main = optimize
--main = runDisp config
