module GL.Shader
  ( compileShader
  , linkProgram
  , ShaderObject
  , ProgramObject
  , useProgram
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Monoid
import Foreign
import Foreign.C
import Graphics.GL.Core45
import Graphics.GL.Types

newtype ShaderObject =
  ShaderObject GLuint


compileShader :: MonadIO m => GLenum -> FilePath -> m ShaderObject
compileShader stage sourceFile =
  liftIO $
  do
    src <-
      readFile sourceFile

    shaderName <-
      glCreateShader stage

    withCString src $ \srcPtr ->
      withArray [srcPtr] $ \srcs ->
        glShaderSource shaderName 1 srcs nullPtr

    glCompileShader shaderName

    compiled <-
      alloca
        ( \ptr ->
            glGetShaderiv shaderName GL_COMPILE_STATUS ptr *> peek ptr )

    unless
      (compiled == GL_TRUE)
      ( do
          putStrLn ("Shader stage failed to compile: " <> show stage)

          logLen <-
            alloca
              ( \ptr ->
                  glGetShaderiv shaderName GL_INFO_LOG_LENGTH ptr *>
                  peek ptr )

          allocaBytes (fromIntegral logLen) $ \infoLogPtr ->
            alloca $ \lenPtr ->
              do
                glGetShaderInfoLog shaderName 1024 lenPtr infoLogPtr

                peekCString infoLogPtr >>= putStrLn )

    pure (ShaderObject shaderName)


newtype ProgramObject =
  ProgramObject GLuint


linkProgram :: (MonadIO m, Foldable t) => t ShaderObject -> m ProgramObject
linkProgram shaders =
  liftIO $
  do
    programName <-
      glCreateProgram

    for_ shaders (\(ShaderObject a) -> glAttachShader programName a)

    glLinkProgram programName

    compiled <-
      alloca (\ptr -> glGetProgramiv programName GL_LINK_STATUS ptr *> peek ptr)

    unless
      (compiled == GL_TRUE)
      ( do
          putStrLn "Program failed to link"

          logLen <-
            alloca
              (\ptr -> glGetProgramiv programName GL_INFO_LOG_LENGTH ptr *> peek ptr)

          allocaBytes (fromIntegral logLen) $ \infoLogPtr ->
            alloca $ \lenPtr ->
              do
                glGetProgramInfoLog programName 1024 lenPtr infoLogPtr

                peekCString infoLogPtr >>= putStrLn)

    pure (ProgramObject programName)


useProgram :: MonadIO m => ProgramObject -> m ()
useProgram (ProgramObject a) = glUseProgram a
