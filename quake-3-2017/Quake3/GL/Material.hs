module Quake3.GL.Material where

import Control.Monad.IO.Class
import Foreign
import GL.BindlessTextures
import GL.ShaderStorageBufferObject
import Graphics.GL.Core45


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
  alignment _ =
    0

  sizeOf ~(Material a b) =
    sizeOf a + sizeOf b

  poke ptr (Material a b) =
    do
      pokeByteOff (castPtr ptr) 0 a
      pokeByteOff (castPtr ptr) (sizeOf a) b



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
  alignment _ =
    0

  sizeOf ~(Pass (a, _, _, _, _) _ k _) =
    sizeOf a * 10 + sizeOf k + 8

  poke ptr (Pass (a, b, c, d, e) (f, g, h, i, j) k l) =
    do
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


{-| Upload a list of materials and their passes to separate material and pass
buffers.
-}
uploadMaterials
  :: MonadIO m
  => [[Pass]] -> m (ShaderStorageBufferObject, ShaderStorageBufferObject)
uploadMaterials materialsWithPasses =
  liftIO $
  do
    materialsBuffer <-
      uploadShaderStorageBufferObject
        ( map
            (\(passes, firstIndex) ->
                Material (fromIntegral (length passes)) (fromIntegral firstIndex))
            (zip
                materialsWithPasses
                (scanl (+) 0 (map length materialsWithPasses))))

    passesBuffer <-
      uploadShaderStorageBufferObject (concat materialsWithPasses)

    return (materialsBuffer, passesBuffer)
