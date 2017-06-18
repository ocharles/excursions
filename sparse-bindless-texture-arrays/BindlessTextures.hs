{-# language NamedFieldPuns #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

{-| A demonstration of ARB_bindless_texture.
-}
module BindlessTextures (main) where

import Codec.Picture
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Managed
import Data.Bits
import Data.Foldable (for_)
import Data.IORef
import Data.Monoid
import Data.StateVar
import qualified Data.Vector.Storable as Vector
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Graphics.GL.Core45
import Graphics.GL.Ext.ARB.BindlessTexture
import Graphics.GL.Ext.ARB.DebugOutput
import Graphics.GL.Ext.ARB.SparseTexture
import Graphics.GL.Types
import qualified SDL
import Text.Printf


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
pattern MAX_TEXTURES :: (Num a, Eq a) => a
pattern MAX_TEXTURES = 2


{-| Instantiate a new texture allocator that can allocate MAX_TEXTURES textures.
-}
newTextureAllocator
  :: MonadIO m
  => GLsizei -> GLsizei -> GLenum -> m TextureAllocator
newTextureAllocator width height internalFormat = liftIO $
  do
    textureName <- alloca $ \ptr ->
      do
        glCreateTextures GL_TEXTURE_2D_ARRAY 1 ptr
        peek ptr

    -- Mark the texture as being sparse
    glTextureParameteri textureName GL_TEXTURE_SPARSE_ARB GL_TRUE

    glTextureStorage3D textureName 1 internalFormat width height MAX_TEXTURES

    nextIndex <- newIORef 0

    return TextureAllocator {..}


{-| Upload a texture with a texture allocator.
-}
loadTexture :: TextureAllocator -> FilePath -> IO ()
loadTexture TextureAllocator { textureName, nextIndex } path =
  do
    imageData <- liftIO (readImage path)

    n <- readIORef nextIndex
    modifyIORef nextIndex succ

    case imageData of
      Left e ->
        error (show e)

      Right (ImageRGB8 (Image width height pixels)) ->
        do
          glBindTexture GL_TEXTURE_2D_ARRAY textureName
          glTexPageCommitmentARB
            GL_TEXTURE_2D_ARRAY
            0
            0
            0
            n
            (fromIntegral width)
            (fromIntegral height)
            1
            GL_TRUE

          Vector.unsafeWith pixels $ \ptr ->
            glTextureSubImage3D
              textureName
              0
              0
              0
              n
              (fromIntegral width)
              (fromIntegral height)
              1
              GL_RGB
              GL_UNSIGNED_BYTE
              (castPtr ptr)



main :: IO ()
main = runManaged $ do
  -- First we need to initialize OpenGL and create a window.
  window <- createOpenGLWindow

  -- Obviously this sample requires ARB_bindless_texture
  unless
    gl_ARB_bindless_texture
    (error "Your graphic card does not support ARB_bindless_texture")

  -- We need ARB_sparse_texture for spare textures
  unless
    gl_ARB_sparse_texture
    (error "Your graphic card does not support ARB_sparse_texture")

  -- Upload a texture to OpenGL. Uses Juicy Pixels to load from disk, and then
  -- glGenTextures/glTexStorage2D/glTexSubImage2D to create and upload the data
  -- to an immutable texture.
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

  glUseProgram program

  -- Create a uniform buffer object, containing our choice of texture.
  uboName <-
    liftIO $ alloca $ \ptr -> do
      glGenBuffers 1 ptr
      peek ptr

  glBindBuffer GL_UNIFORM_BUFFER uboName

  glBufferStorage
    GL_UNIFORM_BUFFER
    4
    nullPtr
    (GL_MAP_PERSISTENT_BIT .|. GL_MAP_WRITE_BIT)

  uboPtr <-
    glMapBufferRange GL_UNIFORM_BUFFER 0 4 (GL_MAP_PERSISTENT_BIT .|. GL_MAP_WRITE_BIT)

  liftIO $ poke (castPtr uboPtr) (1 :: GLfloat)

  glUniformBlockBinding program 0 0
  glBindBufferBase GL_UNIFORM_BUFFER 0 uboName

  -- Draw forever! This will draw a fullscreen triangle with our texture.
  glBindVertexArray vertexArray

  glUniformHandleui64ARB 0 textureHandle

  for_ (cycle [0, 1]) $ \textureIndex -> do
    _ <- SDL.pollEvents

    liftIO $ poke (castPtr uboPtr) (textureIndex :: GLfloat)
    glDrawArrays GL_TRIANGLES 0 3

    SDL.glSwapWindow window


createOpenGLWindow :: Managed SDL.Window
createOpenGLWindow = do
  SDL.initialize [ SDL.InitVideo ]
  window <- managed (bracket createGLWindow SDL.destroyWindow)
  SDL.glCreateContext window >>= SDL.glMakeCurrent window
  installDebugHook
  SDL.swapInterval $= SDL.SynchronizedUpdates
  return window

  where

    createGLWindow =
      let openGLConfig =
            SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Debug 4 5 }

          windowConfig =
            SDL.defaultWindow { SDL.windowOpenGL = Just openGLConfig }

      in  SDL.createWindow "ARB_bindless_texture example" windowConfig


newVertexArray :: Managed GLuint
newVertexArray =
  liftIO $ alloca $ \ptr -> do
    glGenVertexArrays 1 ptr
    peek ptr


compileShader :: MonadIO m => GLenum -> FilePath -> m GLuint
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
    (compiled == GL_TRUE)
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


linkProgram :: (Foldable list, MonadIO m) => list GLuint -> m GLuint
linkProgram shaders = liftIO $ do
  programName <- glCreateProgram

  for_ shaders (glAttachShader programName)

  glLinkProgram programName

  compiled <-
    alloca (\ptr -> glGetProgramiv programName GL_LINK_STATUS ptr *> peek ptr)

  unless
    (compiled == GL_TRUE)
    (do putStrLn "Program failed to link"

        logLen <-
          alloca
            (\ptr -> glGetProgramiv programName GL_INFO_LOG_LENGTH ptr *> peek ptr)

        allocaBytes (fromIntegral logLen) $ \infoLogPtr ->
          alloca $ \lenPtr -> do
            glGetProgramInfoLog programName 1024 lenPtr infoLogPtr
            peekCString infoLogPtr >>= putStrLn)

  pure programName


-- | This will install a synchronous debugging hook to allow OpenGL to notify us
-- if we're doing anything deprecated, non-portable, undefined, etc.
installDebugHook :: MonadIO m => m ()
installDebugHook
  | gl_ARB_debug_output = do
    cb <- liftIO $ mkGLDEBUGPROC glCallback
    glDebugMessageCallbackARB cb nullPtr
    glEnable GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB
  | otherwise = return ()


glCallback :: GLenum
           -> GLenum
           -> GLuint
           -> GLenum
           -> GLsizei
           -> Ptr GLchar
           -> Ptr ()
           -> IO ()
glCallback source t ident severity _ message _ = do
  message' <- peekCString message
  putStrLn $
    printf
      "opengl %s [%s] %s (%s): %s"
      t'
      severity'
      source'
      (show ident)
      message'
  where
    source' :: String
    source' =
      case source of
        GL_DEBUG_SOURCE_API_ARB -> "API"
        GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB -> "Window System"
        GL_DEBUG_SOURCE_SHADER_COMPILER_ARB -> "Shader Compiler"
        GL_DEBUG_SOURCE_THIRD_PARTY_ARB -> "Third Party"
        GL_DEBUG_SOURCE_APPLICATION_ARB -> "Application"
        GL_DEBUG_SOURCE_OTHER_ARB -> "Other"
        _ -> "Unknown"
    t' :: String
    t' =
      case t of
        GL_DEBUG_TYPE_ERROR_ARB -> "Error"
        GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB -> "Deprecated Behaviour"
        GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB -> "Undefined Behaviour"
        GL_DEBUG_TYPE_PORTABILITY_ARB -> "Portability"
        GL_DEBUG_TYPE_PERFORMANCE_ARB -> "Performance"
        GL_DEBUG_TYPE_OTHER_ARB -> "Other"
        _ -> "Unknown"
    severity' :: String
    severity' =
      case severity of
        GL_DEBUG_SEVERITY_HIGH_ARB -> "High"
        GL_DEBUG_SEVERITY_MEDIUM_ARB -> "Medium"
        GL_DEBUG_SEVERITY_LOW_ARB -> "Low"
        _ -> "Unknown"
