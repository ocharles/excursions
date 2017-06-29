{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}

module TextureManager where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.StateVar
import Foreign.Storable
import GL.BindlessTextures
import GL.TextureObject
import Graphics.GL.Core45
import Graphics.GL.Types
import System.FilePath

-- TEXTURE MANAGER


{-| The TextureManager is responsible for loading textures. It implements a
cache to avoid loading textures more than once.
-}
newtype TextureManager = TextureManager
  { textureCache :: IORef (Map FilePath TextureHandle)
  }


{-| Initialize a new texture manager.
-}
newTextureManager
  :: MonadIO m
  => m TextureManager
newTextureManager =
  do
    textureCache <-
      liftIO (newIORef Map.empty)

    return TextureManager {..}


{-| Load a texture from a file path. Throws if the texture cannot be loaded.
-}
loadTexture
  :: (MonadIO m, MonadCatch m)
  => TextureManager -> FilePath -> m TextureHandle
loadTexture (TextureManager cacheRef) filePathWithExtension =
  do
    let
      filePath =
        dropExtension ("../../hs-quake-3/resources/" <> filePathWithExtension)

    oldCache <-
      get cacheRef

    case Map.lookup filePath oldCache of
      Just h ->
        return h

      Nothing ->
        do
          textureObject <-
            tryAndLoad (filePath <.> "jpg")
              `catch` (\SomeException{} -> tryAndLoad (filePath <.> "tga"))
              `catch` (\SomeException{} -> tryAndLoad (filePath <.> "png"))
              `catch` (\SomeException{} -> tryAndLoad filePathWithExtension)

          h <-
            uploadTextureObject textureObject

          cacheRef $~! Map.insert filePath h

          return h

  where

    tryAndLoad filePath =
      do
        imageData <-
          liftIO (readImage filePath)

        case imageData of
          Left e ->
            error (show e)

          Right (ImageRGB8 pixelData) ->
            uploadImage GL_SRGB8 GL_RGB pixelData

          Right (ImageRGBA8 pixelData) ->
            uploadImage GL_SRGB8_ALPHA8 GL_RGBA pixelData

          Right (ImageYCbCr8 img) ->
            uploadImage GL_SRGB8 GL_RGB (convertImage img :: Image PixelRGB8)


{-| Upload local Juicy Pixels data into an OpenGL texture.
-}
uploadImage
  :: (MonadIO m, Storable (PixelBaseComponent pixel))
  => GLuint -> GLuint -> Image pixel -> m TextureObject
uploadImage internalFormat imageFormat (Image width height pixels) =
  do
    width <-
      pure (fromIntegral width)

    height <-
      pure (fromIntegral height)

    texture <-
      newTexture width height internalFormat

    uploadPixels2D texture (0, 0) (width, height) imageFormat pixels

    generateMipmaps texture

    enableLinearFiltering texture

    return texture
