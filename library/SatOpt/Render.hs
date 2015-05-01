{-# LANGUAGE ViewPatterns #-}

-- | TODO
module SatOpt.Render ( Conf (..)
                     , runDisp
                     , KMState --(..)
                     , RGBTrip
                     ) where

import           Control.Concurrent.MVar
import           Control.Monad           (when)
import           Data.Colour.RGBSpace    (RGB, uncurryRGB)
import           Data.List               (transpose)
import           Data.Maybe              (isJust)
import           Foreign                 (withArray)
import           Graphics.UI.GLUT
import           System.Exit             (exitSuccess)

type RGBTrip = (Double, Double, Double)

type KMState = String

data Conf a = Conf { keyBinds :: KMState -> (Conf a) -> (Conf a)
                   , state    :: a
                   , evolve   :: a -> a
                   , render   :: a -> [[RGBTrip]]
                   , canvas   :: (Int, Int)
                   }

imageSize :: Conf a -> TextureSize2D
imageSize (canvas -> (x, y)) = TextureSize2D (fromIntegral x) (fromIntegral y)

(//) :: (Integral a, Fractional b) => a -> a -> b
a // b = fromIntegral a / fromIntegral b

toColor :: (Double, Double, Double) -> Color4 GLubyte
toColor (r, g, b) = Color4 (tc r) (tc g) (tc b) 255
  where
    tc x = round $ x * 255

flatten :: [[RGBTrip]] -> [Color4 GLubyte]
flatten = map toColor . concat

withImage :: Conf a -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withImage c act = withArray arr $ act . PixelData RGBA UnsignedByte
  where
    arr = flatten $ render c (state c)

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
   return mbTexName

display :: Maybe TextureObject -> DisplayCallback
display mbTexName = do
  clear [ ColorBuffer, DepthBuffer ]
  texture Texture2D $= Enabled
  textureFunction $= Decal
  when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f   = vertex   :: Vertex3   GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1.0) (-1.0) 0.0 )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 1.0 (-1.0) 0.0 )
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3 1.0 1.0 0.0 )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3 (-1.0) 1.0 0.0 )
  flush
  texture Texture2D $= Disabled

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
kmEncode k ks m p = show (k, ks, m, p)

changeConf :: (Conf a -> Conf a) -> IO ()
changeConf c = return ()

keyboard :: Conf a -> KeyboardMouseCallback
keyboard _ (Char '\27') Down _ _ = exitSuccess
keyboard c k ks m p = changeConf ((keyBinds c) (kmEncode k ks m p))
--keyboard _            _    _ _ = return ()

setClamping :: TextureCoordName -> Clamping -> IO ()
setClamping coord clamp = do
  textureWrapMode Texture2D coord $= (Repeated, clamp)
  postRedisplay Nothing

--renderThread :: IO MVar

runDisp :: Conf a -> IO ()
runDisp c = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode    $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize     $= Size 250 250
   initialWindowPosition $= Position 100 100
   _ <- createWindow progName
   mbTexName <- myInit c
   displayCallback       $= display mbTexName
   reshapeCallback       $= Just reshape
   keyboardMouseCallback $= Just (keyboard c)
   mainLoop

