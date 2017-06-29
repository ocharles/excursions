{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}

module Quake3.GL.Map
  ( MapResources
  , drawMap
  , bindMap
  , uploadMap
  ) where

import Data.Maybe
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Char8 (unpack)
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as V
import Foreign
import GL.BindlessTextures
import GL.IndirectDrawing
import GL.ShaderStorageBufferObject
import GL.VertexArrayObject
import GL.TextureObject
import Data.Foldable
import Data.Monoid
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear
import Quake3.BSP as Parser
import Quake3.GL.Material
import Quake3.Shader.ShaderRepository
import qualified Quake3.Shader.TypeCheck as TCShader
import TextureManager

-- MAPS


{-| A map, as uploaded to OpenGL. -}
data MapResources = MapResources
  { mapVertexArrayObject :: VertexArrayObject
  , mapMaterials :: ShaderStorageBufferObject
  , mapPasses :: ShaderStorageBufferObject
  , bspFile :: BSPFile
  , mapDrawBuffer :: DrawIndirectBufferObject
  , mapDrawInformation :: ShaderStorageBufferObject
  , sortedFaces :: [Face]
  }


{-| Upload a .bsp map to OpenGL.
-}
uploadMap
  :: (MonadIO m, MonadCatch m)
  => TextureManager -> ShaderRepository -> BSPFile -> m MapResources
uploadMap textureManager shaderRepository bspFile =
  do
    mapVertexArrayObject <-
      uploadMapGeometry bspFile

    mapLightMaps <-
      uploadLightMaps bspFile

    let
      sortedFaces =
        sortOn (faceToLayer bspFile shaderRepository) (GV.toList $ bspFaces bspFile)

    (mapMaterials, mapPasses) <-
      do
        cache <-
          liftIO (newIORef Map.empty)

        passes <-
          for sortedFaces $ \face ->
            let
              texture =
                bspTextures bspFile GV.! fromIntegral (faceTexture face)

            in
              findShader
                cache
                shaderRepository
                textureManager
                (unpack (getASCII (textureName texture)))
                (let
                  index =
                    fromIntegral (faceLMIndex face)

                in
                  if index >= 0
                    then Just (mapLightMaps !! index)
                    else Nothing)

        uploadMaterials passes


    mapDrawBuffer <-
      uploadDrawIndirectCommands (facesToDrawCalls sortedFaces)

    mapDrawInformation <-
      uploadShaderStorageBufferObject
        ( map
            ( \face ->
                DrawInformation
                  { materialIndex = getLittleEndian (faceTexture face)
                  , lightMapIndex = getLittleEndian (faceLMIndex face)
                  }
            )
            sortedFaces)

    return MapResources {..}


uploadLightMaps
  :: MonadIO m
  => BSPFile -> m [TextureHandle]
uploadLightMaps BSPFile {bspLightMaps} =
  liftIO $
  for
    (V.toList bspLightMaps)
    ( \(LightMap pixels) ->
        do
          let
            overBrightPixels :: V.Vector Word8
            overBrightPixels =
              V.fromList $
              concat
                [ let
                    overbright =
                      2

                    colours =
                      [ shiftL
                        (fromIntegral (pixels V.! (j * 3 + n)) :: Int)
                        overbright
                      | n <- [0 .. 2]
                      ]

                  in
                    fmap fromIntegral $
                    if any (> 255) colours
                      then
                        let
                          maxi =
                            maximum colours

                        in
                          fmap (\a -> a * 255 `div` maxi) colours
                      else colours
                | j <- [0 .. 128 * 128 - 1]
                ]

          texture <-
            newTexture 128 128 GL_RGB8

          uploadPixels2D texture (0, 0) (128, 128) GL_RGB overBrightPixels

          uploadTextureObject texture)


{-| Upload vertex and index data for a map.
-}
uploadMapGeometry
  :: MonadIO m
  => BSPFile -> m VertexArrayObject
uploadMapGeometry bspData =
  liftIO $
  do
    vbo <-
      V.unsafeWith (bspVertexes bspData) $ \vData ->
        do
          vbo <-
            alloca $ \ptr ->
              do
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
      alloca $ \ptr ->
        do
          glGenVertexArrays 1 ptr

          peek ptr

    faceIndices <-
      liftIO $
      do
        faceIndices <-
          alloca $ \ptr ->
            do
              glCreateBuffers 1 ptr

              peek ptr

        let
          indices =
            zipWith const [0 :: GLuint .. ] (GV.toList $ bspFaces bspData)

        withArray indices $ \indexData ->
          glNamedBufferData
            faceIndices
            (fromIntegral (length indices * sizeOf (0 :: GLuint)))
            (castPtr indexData)
            GL_STATIC_DRAW

        return faceIndices

    glBindVertexArray vao

    glVertexArrayVertexBuffer
      vao
      0
      vbo
      0
      (fromIntegral (sizeOf (undefined :: Parser.Vertex)))

    glEnableVertexArrayAttrib vao 4

    glBindBuffer GL_ARRAY_BUFFER faceIndices

    glVertexAttribIPointer
      4
      1
      GL_UNSIGNED_INT
      (fromIntegral (sizeOf (0 :: GLuint)))
      nullPtr

    glVertexArrayBindingDivisor
      vao
      4
      1

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
      alloca $ \ptr ->
        do
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
facesToDrawCalls :: [Face] -> [DrawElementsIndirectCommand]
facesToDrawCalls =
  zipWith
    ( \i Face {..} ->
       DrawElementsIndirectCommand
         { deicCount = fromIntegral faceNMeshVerts
         , deicInstanceCount = 1
         , deicFirstIndex = fromIntegral faceMeshVert
         , deicBaseVertex = fromIntegral faceVertex
         , deicBaseInstance = i
         } )
    [0..]



{-| Dispatch draw calls to render a map.
-}
drawMap :: MonadIO m => ShaderRepository -> MapResources -> m ()
drawMap shaderRepository MapResources {..} =
  go (zip [0..] sortedFaces)

  where

    go :: MonadIO m => [(Int, Face)] -> m ()
    go [] =
      return ()

    go ((i, f):fs) =
      do
        let
          (ok, later) =
            span (\(_, g) -> faceState f == faceState g) fs

          (src, dst) =
            faceState f

        glBlendFunc src dst

        drawFaces i (length ok + 1)

        go later

    faceState :: Face -> (GLuint, GLuint)
    faceState face =
      case lookupShader
             shaderRepository
             (unpack
                (getASCII
                   (textureName
                      (bspTextures bspFile GV.! fromIntegral (faceTexture face))))) of
        Just shader ->
          case toList (TCShader._passes shader) of
            [] ->
              (GL_ONE, GL_ZERO)

            (p:_) ->
              case getLast (TCShader._blendFunc p) of
                Nothing ->
                  (GL_ONE, GL_ZERO)

                Just (src, dest) ->
                  (toGL src, toGL dest)

        _ ->
          (GL_ONE, GL_ZERO)

    toGL TCShader.One              = GL_ONE
    toGL TCShader.Zero             = GL_ZERO
    toGL TCShader.DstColor         = GL_DST_COLOR
    toGL TCShader.SrcAlpha         = GL_SRC_ALPHA
    toGL TCShader.OneMinusSrcAlpha = GL_ONE_MINUS_SRC_ALPHA
    toGL TCShader.OneMinusDstAlpha = GL_ONE_MINUS_DST_ALPHA
    toGL TCShader.OneMinusSrcColor = GL_ONE_MINUS_SRC_COLOR
    toGL TCShader.OneMinusDstColor = GL_ONE_MINUS_DST_COLOR
    toGL TCShader.SrcColor         = GL_SRC_COLOR

    drawFaces offset count =
      glMultiDrawElementsIndirect
        GL_TRIANGLES
        GL_UNSIGNED_INT
        (nullPtr `plusPtr` (offset * sizeOf (undefined :: DrawElementsIndirectCommand)))
        (fromIntegral count)
        0


data DrawInformation = DrawInformation
  { materialIndex :: Int32
  , lightMapIndex :: Int32
  }


instance Storable DrawInformation where
  sizeOf ~(DrawInformation a b) =
    sizeOf a + sizeOf b

  alignment _ =
    4

  poke ptr (DrawInformation a b) =
    do
      pokeByteOff (castPtr ptr) 0 a
      pokeByteOff (castPtr ptr) 4 b

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
  for_ formats $ \VertexAttribFormat {..} ->
    do
      glEnableVertexArrayAttrib vao vafAttribute

      glVertexArrayAttribBinding vao vafAttribute 0

      glVertexArrayAttribFormat
        vao
        vafAttribute
        vafComponents
        vafType
        GL_FALSE
        vafOffset

faceToLayer :: BSPFile -> ShaderRepository -> Face -> TCShader.SortLayer
faceToLayer bspFile shaderRepository face =
  case lookupShader
         shaderRepository
         (unpack
            (getASCII
               (textureName
                  (bspTextures bspFile GV.! fromIntegral (faceTexture face))))) of
    Just shader ->
      sortShader shader

    _ ->
      TCShader.Opaque

findShader
  :: (MonadIO m, MonadCatch m)
  => IORef (Map String [Pass])
  -> ShaderRepository
  -> TextureManager
  -> String
  -> Maybe TextureHandle
  -> m [Pass]
findShader loadedRef shaderRepository textureManager shaderName lightmap =
  do
    loaded <-
      liftIO (Data.IORef.readIORef loadedRef)

    case Map.lookup shaderName loaded of
      Just passes ->
        return passes

      Nothing ->
        case lookupShader shaderRepository shaderName of
          Just shader ->
            loadTypeCheckedShader textureManager shader lightmap

          Nothing ->
            createShaderFromTexture
              textureManager
              shaderName
              lightmap


sortShader :: TCShader.Shader -> TCShader.SortLayer
sortShader shader =
  fromMaybe
    ( case toList (TCShader._passes shader) of
        (p:_) ->
          case getLast (TCShader._blendFunc p) of
            Just (TCShader.One, TCShader.Zero) ->
              TCShader.Opaque

            Just _ ->
              TCShader.Blend0

            Nothing ->
              TCShader.Opaque

        _ ->
          TCShader.Opaque )
    (getLast (TCShader._sort shader))


loadTypeCheckedShader textureManager shader lightmap =
  for (toList (TCShader._passes shader)) $ \pass ->
    do
      t <-
        case getLast (TCShader._map pass) of
          Just (TCShader.MapTexture path) ->
            loadTexture textureManager path `catch`
              (\SomeException {} ->
                loadTexture textureManager "test-texture.png")

          Just TCShader.MapLightMap ->
            case lightmap of
              Nothing ->
                loadTexture textureManager "test-texture.png"

              Just lightmap ->
                pure lightmap

          _ ->
            loadTexture textureManager "test-texture.png"

      t <-
        case getLast (TCShader._animMap pass) of
          Just (TCShader.AnimMap _ (path:_)) ->
            loadTexture textureManager path `catch`
              (\SomeException {} ->
                loadTexture textureManager "test-texture.png")

          Nothing ->
            return t

      let
        (sourceFactor, destFactor) =
          case getLast (TCShader._blendFunc pass) of
            Just (src, dst) ->
              (src, dst)

            Nothing ->
              (TCShader.One, TCShader.Zero)

      return
        (Pass
          (blendFuncFactors sourceFactor)
          (blendFuncFactors destFactor)
          t
          (case getLast (TCShader._map pass) of
              Just TCShader.MapLightMap -> 1
              _ -> 0))


createShaderFromTexture textureManager textureName lightmap =
  do
    t <-
      loadTexture textureManager textureName `catch`
        (\SomeException {} -> loadTexture textureManager "test-texture.png")

    return
      (case lightmap of
        Nothing ->
          [ Pass
              (blendFuncFactors TCShader.One)
              (blendFuncFactors TCShader.Zero)
              t
              0]

        Just lightmap ->
          [ Pass
              (blendFuncFactors TCShader.One)
              (blendFuncFactors TCShader.Zero)
              lightmap
              1
          , Pass
              (blendFuncFactors TCShader.DstColor)
              (blendFuncFactors TCShader.Zero)
              t
              0
          ])


blendFuncFactors TCShader.Zero = (0, 0, 0, 0, 0)
blendFuncFactors TCShader.One = (1, 0, 0, 0, 0)
blendFuncFactors TCShader.SrcColor = (0, 1, 0, 0, 0)
blendFuncFactors TCShader.DstColor = (0, 0, 1, 0, 0)
blendFuncFactors TCShader.SrcAlpha = (0, 0, 0, 1, 0)
blendFuncFactors TCShader.OneMinusSrcAlpha = (1, 0, 0, -1, 0)
blendFuncFactors TCShader.OneMinusDstAlpha = (1, 0, 0, 0, -1)

bindMap MapResources{..} =
  do
    bindVertexArray mapVertexArrayObject

    bindShaderStorageBufferObjectToIndex 0 mapMaterials

    bindShaderStorageBufferObjectToIndex 1 mapPasses

    bindShaderStorageBufferObjectToIndex 2 mapDrawInformation

    bindDrawIndirectBuffer mapDrawBuffer
