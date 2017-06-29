{-# language GeneralizedNewtypeDeriving #-}

module GL.BindlessTextures
  ( TextureHandle
  , uploadTextureObject
  ) where

import Control.Monad.IO.Class
import Foreign
import GL.TextureObject
import Graphics.GL.Ext.ARB.BindlessTexture
import Graphics.GL.Types

-- TEXTURE HANDLES


{-| An opaque pointer to a texture in GPU memory.
-}
newtype TextureHandle =
  TextureHandle GLuint64
  deriving (Storable)


{-| Given an OpenGL texture object, return the handle of this texture and make
it resident.
-}
uploadTextureObject
  :: MonadIO m
  => TextureObject -> m TextureHandle
uploadTextureObject texture =
  do
    h <-
      glGetTextureHandleARB (textureObjectName texture)

    glMakeTextureHandleResidentARB h

    return (TextureHandle h)
