module GL.ShaderStorageBufferObject
  ( ShaderStorageBufferObject
  , bindShaderStorageBufferObjectToIndex
  , uploadShaderStorageBufferObject
  ) where

import Control.Monad.IO.Class
import Foreign
import Graphics.GL.Core45
import Graphics.GL.Types


-- SHADER STORAGE BUFFER OBJECTS


newtype ShaderStorageBufferObject =
  ShaderStorageBufferObject GLuint


bindShaderStorageBufferObjectToIndex
  :: MonadIO m
  => GLuint -> ShaderStorageBufferObject -> m ()
bindShaderStorageBufferObjectToIndex n (ShaderStorageBufferObject m) =
  glBindBufferBase GL_SHADER_STORAGE_BUFFER n m


uploadShaderStorageBufferObject contents =
  liftIO $
  do
    bufferName <-
      alloca $ \ptr ->
        do
          glCreateBuffers 1 ptr

          peek ptr

    withArray contents $ \ptr ->
      glNamedBufferData
        bufferName
        (fromIntegral (sizeOf (head contents) * length contents))
        (castPtr ptr)
        GL_STATIC_DRAW

    return (ShaderStorageBufferObject bufferName)
