{-# language OverloadedStrings #-}

{-| A demonstration of ARB_bindless_texture.
-}
module BindlessTextures (main) where

import Control.Concurrent
import Codec.Picture
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Log
import Control.Monad.Managed
import Control.Monad.Trans.Class
import Data.Foldable (for_)
import Data.Monoid
import qualified Data.Vector.Storable as Vector
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek, sizeOf)
import Graphics.GL.Core45
import Graphics.GL.Ext.ARB.BindlessTexture
import Graphics.GL.Types
import qualified SDL
import System.IO (stdout)

main :: IO ()
main = runManaged $ do
  -- First we need to initialize OpenGL and create a window.
  window <- createOpenGLWindow

  -- Obviously this sample requires ARB_bindless_texture
  unless
    gl_ARB_bindless_texture
    (error "Your graphic card does not support ARB_bindless_texture")

  -- Upload a texture to OpenGL. Uses Juicy Pixels to load from disk, and then
  -- glGenTextures/glTexStorage2D/glTexSubImage2D to create and upload the data
  -- to an immutable texture.
  textureId1 <- uploadTexture "test-texture.png"
  textureId2 <- uploadTexture "test-texture-2.png"

  -- ARB_bindless_texture: create a texture handle from our uploaded texture
  -- name.
  textureHandle1 <- glGetTextureHandleARB textureId1
  textureHandle2 <- glGetTextureHandleARB textureId2
  glMakeTextureHandleResidentARB textureHandle1
  glMakeTextureHandleResidentARB textureHandle2

  -- Setup a vertex array for drawing. This uses a fullscreen triangle so
  -- the vertex data itself isn't important.
  vertexArray <- newVertexArray

  -- Load a shader for fullscreen rendering
  program <- do
    vertexShader   <- compileShader GL_VERTEX_SHADER "vertex-shader.glsl"
    fragmentShader <- compileShader GL_FRAGMENT_SHADER "fragment-shader.glsl"
    linkProgram [vertexShader, fragmentShader]

  -- Unbind to prove that this is bindless
  glBindTexture GL_TEXTURE_2D 0

  -- Inform the shader of our texture handle
  glUseProgram program

  -- Draw forever! This will draw a fullscreen triangle with our texture.
  glBindVertexArray vertexArray
  for_ (cycle [textureHandle1, textureHandle2]) $ \textureHandle -> do
    SDL.pollEvents

    glUniformHandleui64ARB 0 textureHandle
    glDrawArrays GL_TRIANGLES 0 3

    SDL.glSwapWindow window


-- createOpenGLWindow :: (MonadIO m) => m SDL.Window
createOpenGLWindow = do
  SDL.initialize [ SDL.InitVideo ]
  window <- managed (bracket createGLWindow SDL.destroyWindow)
  SDL.glCreateContext window >>= SDL.glMakeCurrent window
  return window

  where

    createGLWindow =
      let openGLConfig =
            SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Debug 4 5 }

          windowConfig =
            SDL.defaultWindow { SDL.windowOpenGL = Just openGLConfig }

      in  SDL.createWindow "ARB_bindless_texture example" windowConfig


uploadTexture :: (MonadIO m) => FilePath -> m GLuint
uploadTexture path = do
  imageData <- liftIO (readImage path)
  case imageData of
    Left e                      -> error (show e)
    Right (ImageRGB8 pixelData) -> uploadRGB8 pixelData


newVertexArray =
  liftIO $ alloca $ \ptr -> do
    glGenVertexArrays 1 ptr
    peek ptr


compileShader stage sourceFile = liftIO $ do
  src <- readFile sourceFile

  shaderName <- glCreateShader stage

  withCString src $ \srcPtr ->
    withArray [srcPtr] $ \srcs ->
      glShaderSource shaderName 1 srcs nullPtr

  glCompileShader shaderName

  compiled <-
    alloca
      (\ptr ->
          glGetShaderiv shaderName GL_COMPILE_STATUS ptr *> peek ptr)

  unless
    (fromIntegral compiled == GL_TRUE)
    (do putStrLn ("Shader stage failed to compile: " <> show stage)

        logLen <-
          alloca
            (\ptr ->
                glGetShaderiv shaderName GL_INFO_LOG_LENGTH ptr *>
                peek ptr)

        allocaBytes (fromIntegral logLen) $ \infoLogPtr ->
          alloca $ \lenPtr -> do
            glGetShaderInfoLog shaderName 1024 lenPtr infoLogPtr
            peekCString infoLogPtr >>= putStrLn)

  pure shaderName


linkProgram shaders = liftIO $ do
  programName <- glCreateProgram

  for_ shaders (glAttachShader programName)

  glLinkProgram programName

  compiled <-
    alloca (\ptr -> glGetProgramiv programName GL_LINK_STATUS ptr *> peek ptr)

  unless
    (fromIntegral compiled == GL_TRUE)
    (do putStrLn "Program failed to link"

        logLen <-
          alloca
            (\ptr -> glGetProgramiv programName GL_INFO_LOG_LENGTH ptr *> peek ptr)

        allocaBytes (fromIntegral logLen) $ \infoLogPtr ->
          alloca $ \lenPtr -> do
            glGetProgramInfoLog programName 1024 lenPtr infoLogPtr
            peekCString infoLogPtr >>= putStrLn)

  pure programName


uploadRGB8 :: MonadIO m => Image PixelRGB8 -> m GLuint
uploadRGB8 (Image width height pixels) = liftIO $ do
  name <- alloca $ \namePtr -> do
    glGenTextures 1 namePtr
    peek namePtr
  glBindTexture GL_TEXTURE_2D name
  glTexStorage2D
    GL_TEXTURE_2D
    1
    GL_RGB8
    (fromIntegral width)
    (fromIntegral height)
  Vector.unsafeWith pixels $ \imageData ->
    glTexSubImage2D
      GL_TEXTURE_2D
      0
      0
      0
      (fromIntegral width)
      (fromIntegral height)
      GL_RGB
      GL_UNSIGNED_BYTE
      (castPtr imageData)
  return name
