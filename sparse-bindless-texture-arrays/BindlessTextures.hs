{-# language NamedFieldPuns #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

{-| A demonstration of ARB_bindless_texture.
-}
module BindlessTextures (main) where

import Data.IORef
import Codec.Picture
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Log
import Control.Monad.Managed
import Control.Monad.Trans.Class
import Data.Foldable (for_)
import Data.Monoid
import Data.StateVar
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

{-| A texture allocator as a 2D texture array.

All textures within a texture allocator must have the same "shape" -
dimensions and internal format.
-}
data TextureAllocator = TextureAllocator
  { textureName :: GLuint
  , nextIndex :: IORef GLint
  }


{-| The maximum amount of textures an allocator can hold.
-}
pattern MAX_TEXTURES = 2


{-| Instantiate a new texture allocator that can allocate MAX_TEXTURES textures.
-}
newTextureAllocator width height internalFormat =
  do
    textureName <- alloca $ \ptr ->
      do
        glGenTextures 1 ptr
        peek ptr

    glBindTexture GL_TEXTURE_2D_ARRAY textureName

    glTexStorage3D GL_TEXTURE_2D_ARRAY 1 internalFormat width height MAX_TEXTURES

    nextIndex <- newIORef 0

    return TextureAllocator {..}


{-| Upload a texture with a texture allocator.
-}
-- loadTexture :: TextureAllocator -> FilePath -> IO Texture
loadTexture TextureAllocator { textureName, nextIndex } path =
  do
    imageData <- liftIO (readImage path)

    n <- readIORef nextIndex
    modifyIORef nextIndex succ

    case imageData of
      Left e ->
        error (show e)

      Right (ImageRGB8 (Image width height pixels)) ->
        Vector.unsafeWith pixels $ \imageData ->
          glTexSubImage3D
            GL_TEXTURE_2D_ARRAY
            0
            0
            0
            n
            (fromIntegral width)
            (fromIntegral height)
            1
            GL_RGB
            GL_UNSIGNED_BYTE
            (castPtr imageData)


main :: IO ()
main = runManaged $ do
  -- First we need to initialize OpenGL and create a window.
  window <- createOpenGLWindow

  -- Obviously this sample requires ARB_bindless_texture
  unless
    gl_ARB_bindless_texture
    (error "Your graphic card does not support ARB_bindless_texture")

  textureAllocator <- liftIO (newTextureAllocator 1024 1024 GL_RGB8)
  liftIO (loadTexture textureAllocator "test-texture.png")
  liftIO (loadTexture textureAllocator "test-texture-2.png")

  -- ARB_bindless_texture: create a texture handle from our uploaded texture
  -- name.
  textureHandle <- glGetTextureHandleARB (textureName textureAllocator)
  glMakeTextureHandleResidentARB textureHandle

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

  glUniformHandleui64ARB 0 textureHandle

  for_ (cycle [0, 1]) $ \textureIndex -> do
    SDL.pollEvents

    glUniform1f 1 textureIndex
    glDrawArrays GL_TRIANGLES 0 3

    SDL.glSwapWindow window


-- createOpenGLWindow :: (MonadIO m) => m SDL.Window
createOpenGLWindow = do
  SDL.initialize [ SDL.InitVideo ]
  window <- managed (bracket createGLWindow SDL.destroyWindow)
  SDL.glCreateContext window >>= SDL.glMakeCurrent window
  SDL.swapInterval $= SDL.SynchronizedUpdates
  return window

  where

    createGLWindow =
      let openGLConfig =
            SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Debug 4 5 }

          windowConfig =
            SDL.defaultWindow { SDL.windowOpenGL = Just openGLConfig }

      in  SDL.createWindow "ARB_bindless_texture example" windowConfig


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
