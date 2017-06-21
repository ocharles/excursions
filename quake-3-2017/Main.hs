{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Main where

import Data.Word
import Data.Bits
import Codec.Picture
import Codec.Picture.Types
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Managed hiding (with)
import Data.ByteString.Char8 (unpack)
import Data.Foldable (for_, traverse_, toList)
import Data.IORef (IORef)
import qualified Data.IORef
import Data.Int
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.StateVar
import qualified Data.Text.IO as T
import Data.Traversable
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
import Linear
import Linear.Projection
import Parser
import qualified Quake3.Shader.Parser as ParsedShader
import qualified Quake3.Shader.TypeCheck as TCShader
import qualified SDL
import System.Directory
import System.FilePath
import Text.Megaparsec (parse)
import Text.Printf


-- TEXTURE OBJECTS


{-| A standard OpenGL texture object. -}
newtype TextureObject =
  TextureObject GLuint



-- TEXTURE HANDLES


{-| An opaque pointer to a texture in GPU memory.
-}
newtype TextureHandle =
  TextureHandle GLuint64
  deriving (Storable)


uploadTextureObject
  :: MonadIO m
  => TextureObject -> m TextureHandle
uploadTextureObject (TextureObject textureObjectName) = do
  h <-
    glGetTextureHandleARB textureObjectName

  glMakeTextureHandleResidentARB h

  return (TextureHandle h)



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
newTextureManager = do
  textureCache <-
    newIORef Map.empty

  return TextureManager {..}


{-| Load a texture from a file path. Throws if the texture cannot be loaded.
-}
loadTexture
  :: (MonadIO m, MonadCatch m)
  => TextureManager -> FilePath -> m TextureHandle
loadTexture (TextureManager cacheRef) filePathWithExtension = do
  let filePath = dropExtension ("../../hs-quake-3/resources/" <> filePathWithExtension)

  oldCache <-
    get cacheRef

  case Map.lookup filePath oldCache of
    Just h ->
      return h

    Nothing -> do
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

    tryAndLoad filePath = do
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
  liftIO $ do
    name <-
      alloca $ \namePtr -> do
        glGenTextures 1 namePtr
        peek namePtr

    glBindTexture GL_TEXTURE_2D name

    glTexStorage2D
      GL_TEXTURE_2D
      1
      internalFormat
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
        imageFormat
        GL_UNSIGNED_BYTE
        (castPtr imageData)

    return (TextureObject name)



-- MATERIALS


{-| The OpenGL representation of a material is a pointer into the array of
material passes, along with a counter of how many passes this material
requires.
-}
data Material = Material
  { materialNPasses :: Int32
  , materialFirstPass :: Int32
  }


instance Storable Material where
  alignment _ = 0
  sizeOf ~(Material a b) = sizeOf a + sizeOf b
  poke ptr (Material a b) = do
    pokeByteOff (castPtr ptr) 0 a
    pokeByteOff (castPtr ptr) (sizeOf a) b


-- {-| Given a type-checked Quake 3 shader, compile this down to an OpenGL
-- material.

-- This will also load any required textures
-- -}
-- compileShader :: TextureManager ->



-- SHADER REPOSITORY


{-| A ShaderRepository is a map of parsed and type-checked shaders.
-}
newtype ShaderRepository =
  ShaderRepository (Map String TCShader.Shader)


loadAllShadersInDirectory
  :: MonadIO m
  => FilePath -> m ShaderRepository
loadAllShadersInDirectory directory = do
  m <-
    liftIO $ do
      files <- listDirectory directory
      fmap mconcat $
        for [f | f <- files, takeExtension f == ".shader"] $ \p -> do
          src <- T.readFile ("../../hs-quake-3/resources/scripts" </> p)
          case parse ParsedShader.parseShaderFile p src of
            Left e -> do
              putStrLn $ "Failed to parse " ++ p ++ ": " ++ show e
              return mempty
            Right shaders -> do
              fmap mconcat $
                for shaders $ \s -> do
                  let (typeChecked, warnings) = TCShader.tcShader s
                  traverse_
                    (putStrLn . ((ParsedShader.shaderName s ++ ": ") ++))
                    warnings
                  return (Map.singleton (ParsedShader.shaderName s) typeChecked)
  return (ShaderRepository m)


lookupShader :: ShaderRepository -> String -> Maybe TCShader.Shader
lookupShader (ShaderRepository m) name = Map.lookup name m



-- MAPS


{-| A map, as uploaded to OpenGL. -}
data MapResources = MapResources
  { mapVertexArrayObject :: VertexArrayObject
  , mapMaterials :: ShaderStorageBufferObject
  , mapPasses :: ShaderStorageBufferObject
  , mapDrawCalls :: DrawIndirectBufferObject
  , mapDrawInformation :: ShaderStorageBufferObject
  , mapNDrawCalls :: GLsizei
  , mapLightMaps :: ShaderStorageBufferObject
  }


{-| Upload a .bsp map to OpenGL.
-}
uploadMap
  :: (MonadIO m, MonadCatch m)
  => TextureManager -> ShaderRepository -> BSPFile -> m MapResources
uploadMap textureManager shaderRepository bspFile = do
  mapVertexArrayObject <-
    uploadMapGeometry bspFile

  mapDrawCalls <-
    uploadDrawCalls drawCalls

  mapPassesByTexture <-
    uploadMapShaders textureManager shaderRepository bspFile

  (mapMaterials, mapPasses) <-
    uploadMaterials mapPassesByTexture

  mapDrawInformation <-
    uploadDrawInformation bspFile

  mapLightMaps <-
    uploadLightMaps bspFile

  return MapResources
    { mapNDrawCalls = fromIntegral (length drawCalls)
    , ..
    }

  where

    drawCalls = facesToDrawCalls bspFile


uploadLightMaps
  :: MonadIO m
  => BSPFile -> m ShaderStorageBufferObject
uploadLightMaps BSPFile {bspLightMaps} = do
  handles <-
    liftIO $
    for
      (V.toList bspLightMaps)
      (\(LightMap pixels) -> do
         let overBrightPixels :: V.Vector Word8
             overBrightPixels =
               V.fromList $
               concat $
               [ let overbright = 2
                     colours =
                       [ shiftL
                         (fromIntegral (pixels V.! (j * 3 + n)) :: Int)
                         overbright
                       | n <- [0 .. 2]
                       ]
                 in fmap fromIntegral $
                    if any (> 255) colours
                      then let maxi = maximum colours
                           in fmap (\a -> a * 255 `div` maxi) colours
                      else colours
               | j <- [0 .. 128 * 128 - 1]
               ]
         name <-
           alloca $ \namePtr -> do
             glGenTextures 1 namePtr
             peek namePtr
         glBindTexture GL_TEXTURE_2D name
         glTexStorage2D GL_TEXTURE_2D 1 GL_RGB8 128 128
         Vector.unsafeWith overBrightPixels $ \imageData ->
           glTexSubImage2D
             GL_TEXTURE_2D
             0
             0
             0
             128
             128
             GL_RGB
             GL_UNSIGNED_BYTE
             (castPtr imageData)
         uploadTextureObject (TextureObject name))
  b <-
    liftIO $
    alloca $ \ptr -> do
      glCreateBuffers 1 ptr
      peek ptr
  liftIO $ withArray handles $ \ptr ->
    glNamedBufferData
      b
      (fromIntegral (length handles * sizeOf (head handles)))
      (castPtr ptr)
      GL_STATIC_DRAW
  return (ShaderStorageBufferObject b)


{-| Upload vertex and index data for a map.
-}
uploadMapGeometry
  :: MonadIO m
  => BSPFile -> m VertexArrayObject
uploadMapGeometry bspData =
  liftIO $ do
    vbo <-
      V.unsafeWith (bspVertexes bspData) $ \vData -> do
        vbo <-
          alloca $ \ptr -> do
            glCreateBuffers 1 ptr
            peek ptr
        glNamedBufferData
          vbo
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
        {vafAttribute = 0, vafComponents = 3, vafType = GL_FLOAT, vafOffset = 0}
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
              (sizeOf (undefined :: V3 Float) + sizeOf (undefined :: V2 Float))
        }
      , VertexAttribFormat
        { vafAttribute = 3
        , vafComponents = 3
        , vafType = GL_FLOAT
        , vafOffset =
            fromIntegral
              (sizeOf (undefined :: V3 Float) + sizeOf (undefined :: V2 Float) +
               sizeOf (undefined :: V2 Float))
        }
      ]
    ebo <-
      alloca $ \ptr -> do
        glCreateBuffers 1 ptr
        peek ptr
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
    V.unsafeWith (bspMeshVerts bspData) $ \eboData ->
      glNamedBufferData
        ebo
        (fromIntegral (V.length (bspMeshVerts bspData) * sizeOf (0 :: GLuint)))
        (castPtr eboData)
        GL_STATIC_DRAW
    glVertexArrayElementBuffer vao ebo
    return (VertexArrayObject vao)


{-| Translate individual faces in a BSP file into draw calls.
-}
facesToDrawCalls :: BSPFile -> [DrawElementsIndirectCommand]
facesToDrawCalls bsp =
  map
    (\Face {..} ->
       DrawElementsIndirectCommand
       { deicCount = fromIntegral faceNMeshVerts
       , deicInstanceCount = 1
       , deicFirstIndex = fromIntegral faceMeshVert
       , deicBaseVertex = fromIntegral faceVertex
       , deicBaseInstance = 0
       })
    (GV.toList (bspFaces bsp))


{-| Dispatch draw calls to render a map.
-}
drawMap :: MonadIO m => MapResources -> m ()
drawMap MapResources {..} = do
  bindVertexArray mapVertexArrayObject
  bindDrawIndirectBuffer mapDrawCalls

  bindShaderStorageBufferObjectToIndex 0 mapMaterials
  bindShaderStorageBufferObjectToIndex 1 mapPasses
  bindShaderStorageBufferObjectToIndex 2 mapDrawInformation
  bindShaderStorageBufferObjectToIndex 3 mapLightMaps

  glMultiDrawElementsIndirect
    GL_TRIANGLES
    GL_UNSIGNED_INT
    nullPtr
    mapNDrawCalls
    0


{-| Upload all (referenced) shaders in a map.
-}
uploadMapShaders
  :: (MonadIO m, MonadCatch m)
  => TextureManager -> ShaderRepository -> BSPFile -> m [[Pass]]
uploadMapShaders textureManager shaderRepository bspFile =
  for (GV.toList (bspTextures bspFile)) $ \t -> do
    let textureOrShaderName = unpack (getASCII (textureName t))
    case lookupShader shaderRepository textureOrShaderName of
      Just shader ->
        for (toList (TCShader._passes shader)) $ \pass -> do
          t <-
            case getLast (TCShader._map pass) of
              Just (TCShader.MapTexture path) ->
                loadTexture textureManager path `catch`
                (\SomeException {} ->
                   loadTexture textureManager "test-texture.png")
              _ -> loadTexture textureManager "test-texture.png"
          t <-
            case getLast (TCShader._animMap pass) of
              Just (TCShader.AnimMap _ (path:_)) -> do
                liftIO (print path)
                loadTexture textureManager path `catch`
                  (\SomeException {} ->
                     loadTexture textureManager "test-texture.png")
              Nothing -> return t
          let (sourceFactor, destFactor) =
                case getLast (TCShader._blendFunc pass) of
                  Just (src, dst) -> (src, dst)
                  Nothing -> (TCShader.One, TCShader.Zero)
          return
            (Pass
               (blendFuncFactors sourceFactor)
               (blendFuncFactors destFactor)
               t
               (case getLast (TCShader._map pass) of
                  Just TCShader.MapLightMap -> 1
                  _ -> 0))
      Nothing -> do
        t <-
          loadTexture textureManager textureOrShaderName `catch`
          (\SomeException {} -> loadTexture textureManager "test-texture.png")
        return
          [ Pass
              (blendFuncFactors TCShader.One)
              (blendFuncFactors TCShader.Zero)
              t
              1
          , Pass
              (blendFuncFactors TCShader.DstColor)
              (blendFuncFactors TCShader.Zero)
              t
              0
          ]
  where
    blendFuncFactors TCShader.Zero = (0, 0, 0, 0, 0)
    blendFuncFactors TCShader.One = (1, 0, 0, 0, 0)
    blendFuncFactors TCShader.SrcColor = (0, 1, 0, 0, 0)
    blendFuncFactors TCShader.DstColor = (0, 0, 1, 0, 0)
    blendFuncFactors TCShader.SrcAlpha = (0, 0, 0, 1, 0)
    blendFuncFactors TCShader.OneMinusSrcAlpha = (1, 0, 0, -1, 0)
    blendFuncFactors TCShader.OneMinusDstAlpha = (1, 0, 0, 0, -1)
    blendFuncFactors a = error (show a)


{-| Upload a list of materials and their passes to separate material and pass
buffers.
-}
uploadMaterials
  :: MonadIO m
  => [[Pass]] -> m (ShaderStorageBufferObject, ShaderStorageBufferObject)
uploadMaterials materialsWithPasses =
  liftIO $ do
    materialsBuffer <-
      alloca $ \ptr -> do
        glCreateBuffers 1 ptr
        peek ptr
    let materials =
          map
            (\(passes, firstIndex) ->
               Material (fromIntegral (length passes)) (fromIntegral firstIndex))
            (zip
               materialsWithPasses
               (scanl (+) 0 (map length materialsWithPasses)))
    withArray materials $ \ptr ->
      glNamedBufferData
        materialsBuffer
        (fromIntegral (sizeOf (head materials) * length materials))
        (castPtr ptr)
        GL_STATIC_DRAW
    passesBuffer <-
      alloca $ \ptr -> do
        glCreateBuffers 1 ptr
        peek ptr
    let passes = concat materialsWithPasses
    withArray passes $ \ptr ->
      glNamedBufferData
        passesBuffer
        (fromIntegral (sizeOf (head passes) * length passes))
        (castPtr ptr)
        GL_STATIC_DRAW
    return
      ( ShaderStorageBufferObject materialsBuffer
      , ShaderStorageBufferObject passesBuffer)


data DrawInformation = DrawInformation
  { materialIndex :: Int32
  , lightMapIndex :: Int32
  }


instance Storable DrawInformation where
  sizeOf ~(DrawInformation a b) = sizeOf a + sizeOf b
  alignment _ = 4
  poke ptr (DrawInformation a b) = do
    pokeByteOff (castPtr ptr) 0 a
    pokeByteOff (castPtr ptr) 4 b


{-| Upload draw information for all faces in a map.
-}
uploadDrawInformation
  :: MonadIO m
  => BSPFile -> m ShaderStorageBufferObject
uploadDrawInformation bsp =
  liftIO $ do
    drawInfosBuffer <-
      alloca $ \ptr -> do
        glCreateBuffers 1 ptr
        peek ptr
    let drawInfos =
          map
            (\face ->
               DrawInformation
               { materialIndex = getLittleEndian (faceTexture face)
               , lightMapIndex = getLittleEndian (faceLMIndex face)
               })
            (GV.toList $ bspFaces bsp)
    withArray drawInfos $ \ptr ->
      glNamedBufferData
        drawInfosBuffer
        (fromIntegral (sizeOf (head drawInfos) * length drawInfos))
        (castPtr ptr)
        GL_STATIC_DRAW
    return (ShaderStorageBufferObject drawInfosBuffer)



-- VERTEX ARRAY OBJECTS


newtype VertexArrayObject =
  VertexArrayObject GLuint


bindVertexArray :: MonadIO m => VertexArrayObject -> m ()
bindVertexArray (VertexArrayObject n) = glBindVertexArray n



-- SHADER STORAGE BUFFER OBJECTS


newtype ShaderStorageBufferObject =
  ShaderStorageBufferObject GLuint

bindShaderStorageBufferObjectToIndex
  :: MonadIO m
  => GLuint -> ShaderStorageBufferObject -> m ()
bindShaderStorageBufferObjectToIndex n (ShaderStorageBufferObject m) =
  glBindBufferBase GL_SHADER_STORAGE_BUFFER n m



-- INDIRECT DRAWING


newtype DrawIndirectBufferObject =
  DrawIndirectBufferObject GLuint


uploadDrawCalls
  :: MonadIO m
  => [DrawElementsIndirectCommand] -> m DrawIndirectBufferObject
uploadDrawCalls drawCalls =
  liftIO $ do
    bufferObject <-
      alloca $ \ptr -> do
        glCreateBuffers 1 ptr
        peek ptr
    withArray drawCalls $ \ptr ->
      glNamedBufferData
        bufferObject
        (fromIntegral (sizeOf (head drawCalls) * length drawCalls))
        (castPtr ptr)
        GL_STATIC_DRAW
    return (DrawIndirectBufferObject bufferObject)


bindDrawIndirectBuffer :: MonadIO m => DrawIndirectBufferObject -> m ()
bindDrawIndirectBuffer (DrawIndirectBufferObject n) =
  glBindBuffer GL_DRAW_INDIRECT_BUFFER n


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

  poke ptr (DrawElementsIndirectCommand a b c d e) = do
    poke (castPtr ptr) a
    pokeByteOff (castPtr ptr) (sizeOf a) b
    pokeByteOff (castPtr ptr) (sizeOf b + sizeOf a) c
    pokeByteOff (castPtr ptr) (sizeOf c + sizeOf b + sizeOf a) d
    pokeByteOff (castPtr ptr) (sizeOf d + sizeOf c + sizeOf b + sizeOf a) e



-- PASSES


{-| A single pass used by a material.
-}
data Pass = Pass
  { sourceFactors :: (Int32, Int32, Int32, Int32, Int32)
  , destFactors :: (Int32, Int32, Int32, Int32, Int32)
  , diffuseTexture :: TextureHandle
  , lightMap :: Int32
  }


instance Storable Pass where
  alignment _ = 0
  sizeOf ~(Pass (a, _, _, _, _) _ k l) =
    sizeOf a * 10 + sizeOf k + 8
  poke ptr (Pass (a, b, c, d, e) (f, g, h, i, j) k l) = do
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
    pokeByteOff (castPtr ptr) 48 l



-- MAIN


main :: IO ()
main =
  runManaged $ do
    window <-
      createOpenGLWindow "Quake 3"

    textureManager <-
      newTextureManager

    shaderRepository <-
      loadAllShadersInDirectory "../../hs-quake-3/resources/scripts"

    mapData <-
      loadBSP "../../hs-quake-3/resources/maps/q3dm1.bsp"

    mapResources <-
      liftIO $ uploadMap textureManager shaderRepository mapData

    mapProgram <- do
      vertexShader <-
        compileShader GL_VERTEX_SHADER "vertex-shader.glsl"

      fragmentShader <-
        compileShader GL_FRAGMENT_SHADER "fragment-shader.glsl"

      linkProgram [vertexShader, fragmentShader]

    glClearColor 1 0 0 1
    glUseProgram mapProgram
    liftIO $ withCString "a_position" (glBindAttribLocation mapProgram 0)
    liftIO $ withCString "a_normal" (glBindAttribLocation mapProgram 3)
    liftIO $ withCString "a_uv_0" (glBindAttribLocation mapProgram 1)
    liftIO $ withCString "a_uv_1" (glBindAttribLocation mapProgram 2)
    liftIO $
      with
        (perspective 1.047 (800 / 600) 0.1 5000 !*!
         lookAt (V3 680 100 (-100)) (V3 680 100 (-101)) (V3 0 1 0) :: M44 Float)
        (glUniformMatrix4fv 0 1 GL_TRUE . castPtr)

    glEnable GL_DEPTH_TEST
    glEnable GL_FRAMEBUFFER_SRGB

    forever $ do
      SDL.pollEvents

      glClear GL_DEPTH_BUFFER_BIT

      drawMap mapResources

      SDL.glSwapWindow window


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


newVertexArray :: MonadIO m => m GLuint
newVertexArray =
  liftIO $ alloca $ \ptr -> do
    glGenVertexArrays 1 ptr
    peek ptr


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


newIORef :: MonadIO m => a -> m (IORef a)
newIORef = liftIO . Data.IORef.newIORef
