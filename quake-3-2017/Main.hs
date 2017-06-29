{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Managed hiding (with)
import Data.Bits
import Data.StateVar
import Data.Text (Text)
import Foreign.Marshal.Utils
import Foreign.Ptr
import GL.Debug
import GL.IndirectDrawing
import GL.Shader
import GL.ShaderStorageBufferObject
import GL.VertexArrayObject
import Graphics.GL.Core45
import Linear
import Quake3.BSP as Parser
import Quake3.GL.Map
import Quake3.Logic
import Quake3.Shader.ShaderRepository
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified SDL
import Stopwatch
import TextureManager


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

    mapProgram <-
      do
        vertexShader <-
          compileShader GL_VERTEX_SHADER "vertex-shader.glsl"

        fragmentShader <-
          compileShader GL_FRAGMENT_SHADER "fragment-shader.glsl"

        linkProgram [vertexShader, fragmentShader]

    glClearColor 0 0 0 1

    useProgram mapProgram

    liftIO $
      with
        ( perspective 1.047 (1700 / 900) 0.1 5000 !*!
          lookAt (V3 680 100 (-100)) (V3 680 100 (-101)) (V3 0 1 0) :: M44 Float )
        (glUniformMatrix4fv 0 1 GL_TRUE . castPtr)

    glEnable GL_DEPTH_TEST

    glEnable GL_FRAMEBUFFER_SRGB

    glEnable GL_BLEND

    bindMap mapResources

    (sdlEventAH, onSdlEvent) <-
      liftIO newAddHandler

    (renderAH, onRender) <-
      liftIO newAddHandler

    (stepPhysicsAH, onStepPhysics) <-
      liftIO newAddHandler

    let
      network =
        do
          render <-
            fromAddHandler renderAH

          sdlEvent <-
            fromAddHandler sdlEventAH

          stepPhysics <-
            fromAddHandler stepPhysicsAH

          camera <-
            quake3 (fmap timeSpecToSeconds stepPhysics) sdlEvent

          reactimate $
            renderScene
              <$> pure shaderRepository
              <*> pure mapResources
              <*> fmap (fmap (fmap realToFrac)) camera
              <@ render

    liftIO (compile network >>= actuate)

    stopwatch <-
      newStopwatch

    forever $
      do
        SDL.pollEvents >>= mapM_ (liftIO . onSdlEvent)

        lapStopwatch stopwatch >>= liftIO . onStepPhysics

        liftIO (onRender ())

        SDL.glSwapWindow window




renderScene :: ShaderRepository -> MapResources -> V4 (V4 Float) -> IO ()
renderScene shaderRepository mapResources viewMatrix =
  do
    glClear (GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT)

    with
      (perspective 1.047 (1700 / 900) 0.1 5000 !*! viewMatrix :: M44 Float)
      (glUniformMatrix4fv 0 1 GL_TRUE . castPtr)

    drawMap shaderRepository mapResources


createOpenGLWindow :: Text -> Managed SDL.Window
createOpenGLWindow windowTitle =
  do
    SDL.initialize [ SDL.InitVideo ]

    window <-
      managed (bracket createGLWindow SDL.destroyWindow)

    SDL.glCreateContext window >>= SDL.glMakeCurrent window

    installDebugHook

    SDL.swapInterval $= SDL.SynchronizedUpdates

    _ <-
      SDL.setMouseLocationMode SDL.RelativeLocation

    return window

  where

    createGLWindow =
      let
        openGLConfig =
          SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Debug 4 5 }

        windowConfig =
          SDL.defaultWindow { SDL.windowOpenGL = Just openGLConfig
                            , SDL.windowInitialSize = V2 1700 900}

      in
        SDL.createWindow windowTitle windowConfig
