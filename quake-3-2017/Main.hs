{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Main where

import Codec.Picture.Types
import System.Directory
import System.FilePath
import Data.ByteString.Char8 (unpack)
import Data.Traversable
import Linear
import Linear.Projection
import Parser
import Codec.Picture
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Managed hiding (with)
import Data.Foldable (for_)
import Data.Int
import Data.Monoid
import Data.StateVar
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable as Vector
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable
import Foreign.Storable (peek, sizeOf)
import Graphics.GL.Core45
import Graphics.GL.Ext.ARB.BindlessTexture
import Graphics.GL.Ext.ARB.DebugOutput
import Graphics.GL.Types
import qualified SDL
import Text.Printf

data Material = Material Int32 Int32

instance Storable Material where
  alignment _ = 0
  sizeOf ~(Material a b) = sizeOf a + sizeOf b
  poke ptr (Material a b) = do
    pokeByteOff (castPtr ptr) 0 a
    pokeByteOff (castPtr ptr) (sizeOf a) b


data Pass = Pass
  { sourceFactors :: (Int32, Int32, Int32, Int32, Int32)
  , destFactors :: (Int32, Int32, Int32, Int32, Int32)
  , diffuseTexture :: GLuint64
  }

instance Storable Pass where
  alignment _ = 0
  sizeOf ~(Pass (a, _, _, _, _) _ k) =
    sizeOf a * 10 + sizeOf k
  poke ptr (Pass (a, b, c, d, e) (f, g, h, i, j) k) = do
    pokeByteOff (castPtr ptr) 0 a
    pokeByteOff (castPtr ptr) 4 b
    pokeByteOff (castPtr ptr) 8 c
    pokeByteOff (castPtr ptr) 12 d
    pokeByteOff (castPtr ptr) 16 e
    pokeByteOff (castPtr ptr) 20 f
    pokeByteOff (castPtr ptr) 24 g
    pokeByteOff (castPtr ptr) 28 h
    pokeByteOff (castPtr ptr) 32 i
    pokeByteOff (castPtr ptr) 36 j
    pokeByteOff (castPtr ptr) 40 k

main :: IO ()
main =
  runManaged $ do
    window <- createOpenGLWindow "Quake 3"
    liftIO $ putStrLn "Compiling shaders"
    vertexShader <- compileShader GL_VERTEX_SHADER "vertex-shader.glsl"
    fragmentShader <- compileShader GL_FRAGMENT_SHADER "fragment-shader.glsl"
    program <- linkProgram [vertexShader, fragmentShader]
    liftIO $ putStrLn "Loading BSP"
    bspData <- liftIO $ loadBSP "../../hs-quake-3/resources/maps/q3dm1.bsp"
    liftIO $ putStrLn "Building materials buffer"
    materialsBuffer <-
      liftIO $
      alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_SHADER_STORAGE_BUFFER materialsBuffer
    let materials =
          map (Material 1) $
          map fst $ zip [0 ..] (GV.toList $ bspTextures bspData)
    liftIO $
      withArray materials $ \ptr ->
        glBufferData
          GL_SHADER_STORAGE_BUFFER
          (fromIntegral (sizeOf (head materials) * length materials))
          (castPtr ptr)
          GL_STATIC_DRAW
    let loadTexture p = do
          h <- uploadTexture p >>= glGetTextureHandleARB
          glMakeTextureHandleResidentARB h
          return h
    debugTexture <- loadTexture "test-texture.png"
    blocks <-
      loadTexture
        "../../hs-quake-3/resources/textures/gothic_block/blocks11b.jpg"
    textureHandles <-
      liftIO $
      for (GV.toList $ bspTextures bspData) $ \t -> do
        let name =
              "../../hs-quake-3/resources/" <> unpack (getASCII (textureName t))
        loadTexture (name <.> ".tga") `catch`
          (\(SomeException e) -> loadTexture (name <.> ".jpg")) `catch`
          (\(SomeException e) -> do
             putStrLn ("Missing " ++ show name)
             return debugTexture)
    liftIO $ putStrLn "Building passes buffer"
    passesBuffer <-
      liftIO $
      alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_SHADER_STORAGE_BUFFER passesBuffer
    let passes =
          Pass (1, 0, 0, 0, 0) (0, 0, 0, 0, 0) debugTexture :
          Pass (1, 0, 0, 0, 0) (0, 0, 0, 0, 0) blocks :
          map (Pass (1, 0, 0, 0, 0) (0, 0, 0, 0, 0)) textureHandles
    liftIO $
      withArray passes $ \ptr ->
        glBufferData
          GL_SHADER_STORAGE_BUFFER
          (fromIntegral (sizeOf (head passes) * length passes))
          (castPtr ptr)
          GL_STATIC_DRAW
    let faces =
          map
            (\(LeafFace (LittleEndian n)) ->
               bspFaces bspData GV.! fromIntegral n)
            (V.toList $ bspLeafFaces bspData)
    liftIO $ putStrLn "Building draw info buffer"
    drawInfosBuffer <-
      liftIO $
      alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_SHADER_STORAGE_BUFFER drawInfosBuffer
    let drawInfos = map (getLittleEndian . faceTexture) faces
    liftIO $
      withArray drawInfos $ \ptr ->
        glBufferData
          GL_SHADER_STORAGE_BUFFER
          (fromIntegral (sizeOf (head drawInfos) * length drawInfos))
          (castPtr ptr)
          GL_STATIC_DRAW
    glBindBufferBase GL_SHADER_STORAGE_BUFFER 0 materialsBuffer
    glBindBufferBase GL_SHADER_STORAGE_BUFFER 1 passesBuffer
    glBindBufferBase GL_SHADER_STORAGE_BUFFER 2 drawInfosBuffer
    liftIO $ putStrLn "Uploading map"
    bsp <-
      liftIO $ do
        vbo <-
          V.unsafeWith (bspVertexes bspData) $ \vData -> do
            vbo <-
              alloca $ \ptr -> do
                glGenBuffers 1 ptr
                peek ptr
            glBindBuffer GL_ARRAY_BUFFER vbo
            glBufferData
              GL_ARRAY_BUFFER
              (fromIntegral
                 (V.length (bspVertexes bspData) *
                  sizeOf (V.head (bspVertexes bspData))))
              (castPtr vData)
              GL_STATIC_DRAW
            return vbo
        vao <-
          alloca $ \ptr -> do
            glGenVertexArrays 1 ptr
            peek ptr
        glBindVertexArray vao
        glVertexArrayVertexBuffer
          vao
          0
          vbo
          0
          (fromIntegral (sizeOf (undefined :: Parser.Vertex)))
        configureVertexArrayAttributes
          vao
          [ VertexAttribFormat
            { vafAttribute = 0
            , vafComponents = 3
            , vafType = GL_FLOAT
            , vafOffset = 0
            }
          , VertexAttribFormat
            { vafAttribute = 1
            , vafComponents = 2
            , vafType = GL_FLOAT
            , vafOffset = fromIntegral (sizeOf (undefined :: V3 Float))
            }
          , VertexAttribFormat
            { vafAttribute = 2
            , vafComponents = 2
            , vafType = GL_FLOAT
            , vafOffset =
                fromIntegral
                  (sizeOf (undefined :: V3 Float) +
                   sizeOf (undefined :: V2 Float))
            }
          , VertexAttribFormat
            { vafAttribute = 3
            , vafComponents = 3
            , vafType = GL_FLOAT
            , vafOffset =
                fromIntegral
                  (sizeOf (undefined :: V3 Float) +
                   sizeOf (undefined :: V2 Float) +
                   sizeOf (undefined :: V2 Float))
            }
          ]
        ebo <-
          alloca $ \ptr -> do
            glGenBuffers 1 ptr
            peek ptr
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
        V.unsafeWith (bspMeshVerts bspData) $ \eboData ->
          glBufferData
            GL_ELEMENT_ARRAY_BUFFER
            (fromIntegral
               (V.length (bspMeshVerts bspData) * sizeOf (0 :: GLuint)))
            (castPtr eboData)
            GL_STATIC_DRAW
        glVertexArrayElementBuffer vao ebo
        return vao
    glUseProgram program
    glBindVertexArray bsp
    liftIO $ withCString "a_position" (glBindAttribLocation program 0)
    liftIO $ withCString "a_normal" (glBindAttribLocation program 3)
    liftIO $ withCString "a_uv_0" (glBindAttribLocation program 1)
    liftIO $ withCString "a_uv_1" (glBindAttribLocation program 2)
    liftIO $
      with
        (perspective 1.047 (800 / 600) 0.1 5000 !*!
         lookAt (V3 500 10 (-400)) (V3 500 10 (-401)) (V3 0 1 0) :: M44 Float)
        (glUniformMatrix4fv 0 1 GL_TRUE . castPtr)
    glEnable GL_DEPTH_TEST
    drawCommands <-
      liftIO $
      alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    glBindBuffer GL_DRAW_INDIRECT_BUFFER drawCommands
    let daics =
          map
            (\Face {..} ->
               DrawElementsIndirectCommand
               { deicCount = fromIntegral faceNMeshVerts
               , deicInstanceCount = 1
               , deicFirstIndex = fromIntegral faceMeshVert
               , deicBaseVertex = fromIntegral faceVertex
               , deicBaseInstance = 0
               })
            faces
    liftIO $
      withArray daics $ \ptr ->
        glBufferData
          GL_DRAW_INDIRECT_BUFFER
          (fromIntegral (sizeOf (head daics) * length daics))
          (castPtr ptr)
          GL_STATIC_DRAW
    forever $ do
      SDL.pollEvents
      glClearColor 1 0 0 1
      glClear (GL_DEPTH_BUFFER_BIT)
      glMultiDrawElementsIndirect
        GL_TRIANGLES
        GL_UNSIGNED_INT
        nullPtr
        (fromIntegral (length daics))
        0
      SDL.glSwapWindow window
    return ()



createOpenGLWindow windowTitle = do
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

      in  SDL.createWindow windowTitle windowConfig

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

uploadTexture :: (MonadIO m) => FilePath -> m GLuint
uploadTexture path = do
  imageData <- liftIO (readImage path)
  case imageData of
    Left e                      -> error (show e)
    Right (ImageRGB8 pixelData) -> uploadRGB8 pixelData
    Right (ImageYCbCr8 img) ->
      uploadRGB8 (convertImage img :: Image PixelRGB8)



newVertexArray :: MonadIO m => m GLuint
newVertexArray =
  liftIO $ alloca $ \ptr -> do
    glGenVertexArrays 1 ptr
    peek ptr


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

data VertexAttribFormat = VertexAttribFormat
  { vafAttribute :: GLuint
  , vafComponents :: GLsizei
  , vafType :: GLenum
  , vafOffset ::GLuint
  }

configureVertexArrayAttributes
  :: MonadIO m
  => GLuint -> [VertexAttribFormat] -> m ()
configureVertexArrayAttributes vao formats =
  for_ formats $ \VertexAttribFormat {..} -> do
    glEnableVertexArrayAttrib vao vafAttribute
    glVertexArrayAttribBinding vao vafAttribute 0
    glVertexArrayAttribFormat
      vao
      vafAttribute
      vafComponents
      vafType
      (fromIntegral GL_FALSE)
      vafOffset

data DrawElementsIndirectCommand = DrawElementsIndirectCommand
  { deicCount :: GLuint
  , deicInstanceCount :: GLuint
  , deicFirstIndex :: GLuint
  , deicBaseVertex :: GLuint
  , deicBaseInstance :: GLuint
  }


instance Storable DrawElementsIndirectCommand where
  sizeOf ~(DrawElementsIndirectCommand a b c d e) =
    sizeOf a + sizeOf b + sizeOf c + sizeOf d + sizeOf e

  alignment _ = 0

  poke ptr (DrawElementsIndirectCommand a b c d e) =
    do
      poke
        (castPtr ptr)
        a
      pokeByteOff
        (castPtr ptr)
        (sizeOf a)
        b
      pokeByteOff
        (castPtr ptr)
        (sizeOf b + sizeOf a)
        c
      pokeByteOff
        (castPtr ptr)
        (sizeOf c + sizeOf b + sizeOf a)
        d
      pokeByteOff
        (castPtr ptr)
        (sizeOf d + sizeOf c + sizeOf b + sizeOf a)
        e
