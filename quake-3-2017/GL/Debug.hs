module GL.Debug (installDebugHook) where

import Control.Monad.IO.Class
import Foreign.C.String
import Foreign.Ptr
import Graphics.GL.Core45
import Graphics.GL.Ext.ARB.DebugOutput
import Graphics.GL.Types
import Text.Printf

installDebugHook :: MonadIO m => m ()
installDebugHook
  | gl_ARB_debug_output =
      do
        cb <-
          liftIO $ mkGLDEBUGPROC glCallback

        glDebugMessageCallbackARB cb nullPtr

        glEnable GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB

  | otherwise =
      return ()


glCallback :: GLenum
           -> GLenum
           -> GLuint
           -> GLenum
           -> GLsizei
           -> Ptr GLchar
           -> Ptr ()
           -> IO ()
glCallback source t ident severity _ message _ =
  do
    message' <-
      peekCString message

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
        GL_DEBUG_SOURCE_API_ARB ->
          "API"

        GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB ->
          "Window System"

        GL_DEBUG_SOURCE_SHADER_COMPILER_ARB ->
          "Shader Compiler"

        GL_DEBUG_SOURCE_THIRD_PARTY_ARB ->
          "Third Party"

        GL_DEBUG_SOURCE_APPLICATION_ARB ->
          "Application"

        GL_DEBUG_SOURCE_OTHER_ARB ->
          "Other"

        _ ->
          "Unknown"

    t' :: String
    t' =
      case t of
        GL_DEBUG_TYPE_ERROR_ARB ->
          "Error"

        GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB ->
          "Deprecated Behaviour"

        GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB ->
          "Undefined Behaviour"

        GL_DEBUG_TYPE_PORTABILITY_ARB ->
          "Portability"

        GL_DEBUG_TYPE_PERFORMANCE_ARB ->
          "Performance"

        GL_DEBUG_TYPE_OTHER_ARB ->
          "Other"

        _ ->
          "Unknown"

    severity' :: String
    severity' =
      case severity of
        GL_DEBUG_SEVERITY_HIGH_ARB ->
          "High"

        GL_DEBUG_SEVERITY_MEDIUM_ARB ->
          "Medium"

        GL_DEBUG_SEVERITY_LOW_ARB ->
          "Low"

        _ ->
          "Unknown"
