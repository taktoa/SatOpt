{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}

-- | TODO
module SatOpt.Render ( Conf (..)
                     , runDisp
                     , KMState --(..)
                     , RGBTrips
                     ) where

import           Control.Concurrent.MVar
import           Control.Monad                 (when)
import           Data.Maybe                    (isJust)
import           Data.Packed.Vector            (buildVector)
import           Foreign                       (withArray)
import           Graphics.UI.GLUT
import           Numeric.LinearAlgebra.HMatrix hiding (reshape)
import           System.Exit                   (exitSuccess)

type Mℝ = Numeric.LinearAlgebra.HMatrix.Matrix ℝ

type RGBTrips = (Mℝ, Mℝ, Mℝ)

type KMState = String

data Conf a = Conf { keyBinds :: KMState -> (Conf a) -> IO (Conf a)
                   , state    :: a
                   , evolve   :: a -> a
                   , render   :: a -> RGBTrips
                   , canvas   :: (Int, Int)
                   }

imageSize :: Conf a -> TextureSize2D
imageSize (canvas -> (x, y)) = TextureSize2D (fromIntegral x) (fromIntegral y)

(//) :: (Integral a, Fractional b) => a -> a -> b
a // b = fromIntegral a / fromIntegral b

toColor :: Double -> Double -> Double -> Color4 GLubyte
toColor r g b = Color4 (tc r) (tc g) (tc b) 255
  where
    tc = round . (*250)

mapVecs3 :: (Container Vector a, Indexable (Vector a) a, Element b)
            => (a -> a -> a -> b)
            -> Vector a
            -> Vector a
            -> Vector a
            -> Vector b
mapVecs3 f x y z = buildVector (size x) (\i -> f (x ! i) (y ! i) (z ! i))

instance Element GLubyte
instance Element (Color4 GLubyte)

flattenImage :: RGBTrips -> Vector (Color4 GLubyte)
flattenImage (flatten -> r, flatten -> g, flatten -> b)
  = mapVecs3 toColor r g b

withImage :: Conf a -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withImage c act = withArray arr $ act . PixelData RGBA UnsignedByte
  where
    arr = toList $ flattenImage $ render c (state c)

myInit :: Conf a -> IO (Maybe TextureObject)
myInit c = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   depthFunc $= Just Less
   rowAlignment Unpack $= 1

   exts <- get glExtensions
   mbTexName <- if "GL_EXT_texture_object" `elem` exts
                   then fmap Just genObjectName
                   else return Nothing
   when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName

   textureWrapMode Texture2D S $= (Repeated, Repeat)
   textureWrapMode Texture2D T $= (Repeated, Repeat)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   withImage c $ texImage2D Texture2D NoProxy 0 RGBA' (imageSize c) 0
   texture Texture2D $= Enabled
   return mbTexName

evolveC :: Conf a -> Conf a
evolveC c = c { state = evolve c $ state c }

display :: MVar (Conf a) -> Maybe TextureObject -> DisplayCallback
display mv mbTexName = do
  cstate <- readMVar mv
  clear [ ColorBuffer, DepthBuffer ]
  textureFunction $= Decal
  when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f   = vertex   :: Vertex3   GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1.0) (-1.0) 0.0 )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 1.0 (-1.0) 0.0 )
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3 1.0 1.0 0.0 )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3 (-1.0) 1.0 0.0 )
  withImage cstate $ texImage2D Texture2D NoProxy 0 RGBA' (imageSize cstate) 0
  swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 1 1
  matrixMode $= Modelview 0
  loadIdentity
  translate (Vector3 0 0 (-1 :: GLfloat))

kmEncode :: Key -> KeyState -> Modifiers -> Position -> KMState
kmEncode (Char k)        Down _ _ = [k]
kmEncode (SpecialKey k)  Down _ _ = show k
kmEncode (MouseButton m) Down _ _ = show m
kmEncode _ _ _ _ = ""

keyboard :: MVar (Conf a) -> KeyboardMouseCallback
keyboard _ (Char '\27') Down _ _ = exitSuccess
keyboard mv k ks m p = do
  c <- readMVar mv
  let e = kmEncode k ks m p
  modifyMVar_ mv (keyBinds c e)

idle :: MVar (Conf a) -> IdleCallback
idle mv = do
  modifyMVar_ mv (return . evolveC)
  postRedisplay Nothing

runDisp :: Conf a -> IO ()
runDisp c = do
  cstate <- newMVar c
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode    $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
  initialWindowSize     $= Size 250 250
  initialWindowPosition $= Position 100 100
  _ <- createWindow progName
  tex <- myInit c
  displayCallback       $= display cstate tex
  reshapeCallback       $= Just reshape
  keyboardMouseCallback $= Just (keyboard cstate)
  idleCallback          $= Just (idle cstate)
  mainLoop







{-


module Main where

    import Graphics.GPipe
    import qualified Data.Vec as Vec
    import Data.Vec.Nat
    import Data.Vec.LinAlg.Transform3D
    import Data.Monoid
    import Data.IORef
    import Foreign.C.Types
    import Foreign.Marshal.Array
    import Graphics.UI.GLUT( Window,
                             mainLoop,
                             postRedisplay,
                             idleCallback,
                             getArgsAndInitialize,
                             ($=))

    triVerts = [ (-0.8):.0.8:.0.0:.(),
             0.8:.0.8:.0.0:.(),
             0.0:.(-0.8):.0.0:.() ]
    uvCoords = [ 0.0:.0.0:.(),
                 1.0:.0.0:.(),
                 0.5:.1.0:.() ]

    triangle :: PrimitiveStream Triangle (Vec3 (Vertex Float),Vec2 (Vertex Float))
    triangle = toGPUStream TriangleList $ zip triVerts uvCoords

    -- This implements the vertex shader
    procTriangle :: Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), Vec2 (Vertex Float))
    procTriangle size = fmap (projTriangle size) triangle

    projTriangle :: Vec2 Int -> (Vec3 (Vertex Float),Vec2 (Vertex Float)) -> (Vec4 (Vertex Float), Vec2 (Vertex Float))
    projTriangle size (pos,uv) = (orthoProj `multmv` homPos, uv)
        where homPos = homPoint pos :: Vec4 (Vertex Float)
              orthoProj = toGPU $ orthogonal (-10) 10  (2:.2:.())

    -- This implements the fragment shader
    rastTriangle :: Texture2D RGBFormat -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
    rastTriangle tex size = fmap (applyTex tex) $ rasterizeFrontAndBack $ procTriangle size

    applyTex tex (front,uv) = sample (Sampler Linear Wrap) tex uv

    triangleFrame :: Texture2D RGBFormat -> Vec2 Int -> FrameBuffer RGBFormat () ()
    triangleFrame tex size = draw (rastTriangle tex size) clear
        where
          draw  = paintColor NoBlending (RGB $ Vec.vec True)
          clear = newFrameBufferColor (RGB (0.1:.0.3:.0.6:.()))

    main :: IO ()
    main = do
      getArgsAndInitialize
      putStrLn "loading texture..."
      tex <- loadTexture demonHead
      putStrLn "creating window..."
      newWindow "Green Triangle"
           (100:.100:.())
           (800:.600:.())
           (renderFrame tex)
           initWindow
      putStrLn "entering mainloop..."
      mainLoop

    renderFrame :: Texture2D RGBFormat -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
    renderFrame tex size = do
              return $ triangleFrame tex size

    initWindow :: Window -> IO ()
    initWindow win = idleCallback $= Nothing --Just (postRedisplay (Just win))


    loadTexture' ptr = newTexture (PerComp3 UnsignedByteFormat) RGB8 (128:.128:.()) [ptr]
    loadTexture = flip withArray loadTexture'

    demonHead :: [CUChar]

-}
