module GL.IndirectDrawing
  ( DrawIndirectBufferObject
  , bindDrawIndirectBuffer
  , DrawElementsIndirectCommand(..)
  , uploadDrawIndirectCommands
  ) where

import Foreign
import Control.Monad.IO.Class
import Graphics.GL.Core45
import Graphics.GL.Types

-- INDIRECT DRAWING


newtype DrawIndirectBufferObject =
  DrawIndirectBufferObject GLuint


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

  alignment _ =
    0

  poke ptr (DrawElementsIndirectCommand a b c d e) =
    do
      poke (castPtr ptr) a

      pokeByteOff (castPtr ptr) (sizeOf a) b

      pokeByteOff (castPtr ptr) (sizeOf b + sizeOf a) c

      pokeByteOff (castPtr ptr) (sizeOf c + sizeOf b + sizeOf a) d

      pokeByteOff (castPtr ptr) (sizeOf d + sizeOf c + sizeOf b + sizeOf a) e

uploadDrawIndirectCommands commands =
  liftIO $
  do
    name <-
      alloca $ \ptr ->
        do
          glCreateBuffers 1 ptr

          peek ptr

    withArray commands $ \ptr ->
      glNamedBufferData
        name
        (fromIntegral (length commands * sizeOf (undefined :: DrawElementsIndirectCommand)))
        (castPtr ptr)
        GL_STATIC_DRAW

    return (DrawIndirectBufferObject name)
