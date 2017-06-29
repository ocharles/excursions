module GL.VertexArrayObject
  ( VertexArrayObject(..)
  , bindVertexArray
  ) where

import Control.Monad.IO.Class
import Graphics.GL.Core45
import Graphics.GL.Types

-- VERTEX ARRAY OBJECTS


newtype VertexArrayObject =
  VertexArrayObject GLuint


bindVertexArray :: MonadIO m => VertexArrayObject -> m ()
bindVertexArray (VertexArrayObject n) =
  glBindVertexArray n
