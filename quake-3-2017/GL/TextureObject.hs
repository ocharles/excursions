module GL.TextureObject
  ( TextureObject
  , textureObjectName
  , newTexture
  , enableLinearFiltering
  , generateMipmaps
  , uploadPixels2D
  ) where

import Control.Monad.IO.Class
import qualified Data.Vector.Storable as Vector
import Foreign
import Graphics.GL.Core45
import Graphics.GL.Types

-- TEXTURE OBJECTS


{-| A standard OpenGL texture object. -}
newtype TextureObject =
  TextureObject { textureObjectName :: GLuint }

newTexture width height internalFormat =
  liftIO $
  do
    name <-
      alloca $ \namePtr ->
        do
          glCreateTextures GL_TEXTURE_2D 1 namePtr

          peek namePtr

    glTextureStorage2D
      name
      (floor (logBase 2 (fromIntegral (max width height) :: Float)))
      internalFormat
      width
      height

    return (TextureObject name)


generateMipmaps (TextureObject name) =
    glGenerateTextureMipmap name


enableLinearFiltering (TextureObject name) =
  do
    glTextureParameteri
      name
      GL_TEXTURE_MIN_FILTER
      GL_LINEAR_MIPMAP_LINEAR

    glTextureParameteri
      name
      GL_TEXTURE_MAG_FILTER
      GL_LINEAR


uploadPixels2D (TextureObject name) (x, y) (width, height) imageFormat pixels =
  liftIO $
  Vector.unsafeWith pixels $ \imageData ->
    glTextureSubImage2D
      name
      0
      x
      y
      width
      height
      imageFormat
      GL_UNSIGNED_BYTE
      (castPtr imageData)
