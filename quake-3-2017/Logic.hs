{-# language NamedFieldPuns #-}

module Logic where

import Linear
import Reactive.Banana
import qualified SDL.Event as SDL
import qualified SDL.Input as SDL

instance Num a => Num (Behavior a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate
  (-) = liftA2 (-)

{-| quake3 takes a clock and stream of SDL events and interprets them into Quake 3
scenes.
-}
quake3
  :: MonadMoment m
  => Event Double -> Event SDL.Event -> m (Behavior (M44 Double))
quake3 physicsSteps sdlEvents =
  do
    let
      sdlEventPayloads =
        fmap SDL.eventPayload sdlEvents

      keyboardEvents =
        filterJust
          (fmap asKeyboardEvent sdlEventPayloads)

      mouseMotionEvents =
        filterJust
          (fmap asMouseMotionEvent sdlEventPayloads)

    -- The rotation of the FPS camera. Instantaneously updates based on mouse
    -- motion (that is, independently of the physics clock). Stored as radians.
    pitchAndYaw <-
      let
        pixelMotionToRadians (V2 x y) =
          negate (V2 (fromIntegral x) (fromIntegral y)) * 0.001

        update mouseMotionEvent old =
          old + pixelMotionToRadians (SDL.mouseMotionEventRelMotion mouseMotionEvent)

      in
        accumB (V2 0 0) (fmap update mouseMotionEvents)

    let
      anglesToRotation (V2 x y) =
        axisAngle (V3 0 1 0) x * axisAngle (V3 1 0 0) y

      cameraRotation =
        fmap anglesToRotation pitchAndYaw

    let
      forwardVector =
        rotate <$> cameraRotation <*> pure (V3 0 0 (-1))

      rightVector =
        fmap
          (\(V2 x _) ->
             let
               rotation =
                 axisAngle (V3 0 1 0) x

             in
               rotate rotation (V3 0 0 (-1)))
          pitchAndYaw

    forwardMotion <-
      do
        let
          justScancode k e =
            if SDL.keysymScancode (SDL.keyboardEventKeysym e) == k
              then Just e
              else Nothing

          filterKeyEvents k =
            filterJust
              (fmap (justScancode k) keyboardEvents)

          wKeyEvents =
            filterKeyEvents SDL.ScancodeW

          sKeyEvents =
            filterKeyEvents SDL.ScancodeS

          keyState =
            stepper SDL.Released . fmap SDL.keyboardEventKeyMotion

        wState <-
          keyState wKeyEvents

        sState <-
          keyState sKeyEvents

        let
          keyStateToSpeed state =
            case state of
              SDL.Pressed ->
                1

              SDL.Released ->
                0

        pure
          (100 * (fmap keyStateToSpeed wState - fmap keyStateToSpeed sState))

    cameraPosition <-
      accumB
        (V3 680 100 (-100))
        (let
           update forwardVector dt oldPosition =
             oldPosition + forwardVector ^* dt

         in
           update <$> (liftA2 (^*) forwardVector forwardMotion) <@> physicsSteps)

    pure
      (let
         makeCamera position forwardVector orientation =
           lookAt
             position
             (position + forwardVector)
             (rotate orientation (V3 0 1 0))

      in
        makeCamera <$> cameraPosition <*> forwardVector <*> cameraRotation)


asKeyboardEvent :: SDL.EventPayload -> Maybe SDL.KeyboardEventData
asKeyboardEvent e =
  case e of
    SDL.KeyboardEvent e ->
      Just e

    _ ->
      Nothing


asMouseMotionEvent :: SDL.EventPayload -> Maybe SDL.MouseMotionEventData
asMouseMotionEvent e =
  case e of
    SDL.MouseMotionEvent e ->
      Just e

    _ ->
      Nothing
