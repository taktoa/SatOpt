-- | TODO
module SatOpt.Render ( Config
                     , ImageFunc
                     , runDisp
                     ) where

import           Control.Monad    (when)
import           Data.Maybe       (isJust)
import           Foreign          (withArray)
import           Graphics.UI.GLUT
import           System.Exit      (exitSuccess)

type ImageFunc = Double -> Double -> (Double, Double, Double)
type Config = (Int, Int, ImageFunc)

imageFunc :: Config -> ImageFunc
imageFunc (_,_,i) = i

imageSize :: Config -> TextureSize2D
imageSize (x,y,_) = TextureSize2D (fromIntegral x) (fromIntegral y)

(//) :: (Integral a, Fractional b) => a -> a -> b
a // b = fromIntegral a / fromIntegral b

toColor :: (Double, Double, Double) -> Color4 GLubyte
toColor (r, g, b) = Color4 (tc r) (tc g) (tc b) 255
  where
    tc x = round $ x * 255

withImage :: ImageFunc -> TextureSize2D -> (GLubyte -> Color4 GLubyte)
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withImage imf (TextureSize2D w h) f act =
   withArray [ toColor c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c = imf (i // w) (j // h) ] $
   act. PixelData RGBA UnsignedByte

myInit :: Config -> IO (Maybe TextureObject)
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
   withImage (imageFunc c) (imageSize c) (\col -> Color4 col col col 255) $
      texImage2D Texture2D NoProxy 0  RGBA' (imageSize c) 0
   return mbTexName

display ::  Maybe TextureObject -> DisplayCallback
display mbTexName = do
   clear [ ColorBuffer, DepthBuffer ]
   texture Texture2D $= Enabled
   textureFunction $= Decal
   when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName

   -- resolve overloading, not needed in "real" programs
   let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()
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

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitSuccess
keyboard _            _    _ _ = return ()

setClamping :: TextureCoordName -> Clamping -> IO ()
setClamping coord clamp = do
   textureWrapMode Texture2D coord $= (Repeated, clamp);
   postRedisplay Nothing

runDisp :: Config -> IO ()
runDisp c = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   _ <- createWindow progName
   mbTexName <- myInit c
   displayCallback $= display mbTexName
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
