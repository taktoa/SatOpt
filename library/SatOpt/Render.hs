{-# LANGUAGE ViewPatterns #-}

-- | TODO
module SatOpt.Render ( Conf (..)
                     , runDisp
                     , KMState --(..)
                     , RGBTrip
                     ) where

import           Control.Concurrent.MVar
import           Control.Monad           (when)
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
kmEncode k ks m p = show (k, ks, m, p)

changeConf :: MVar (Conf a) -> (Conf a -> Conf a) -> IO ()
changeConf mv ch = modifyMVar_ mv (return . ch)

keyboard :: MVar (Conf a) -> KeyboardMouseCallback
keyboard _ (Char '\27') Down _ _ = exitSuccess
keyboard mv k ks m p = do
  c <- readMVar mv
  changeConf mv $ keyBinds c $ kmEncode k ks m p

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

