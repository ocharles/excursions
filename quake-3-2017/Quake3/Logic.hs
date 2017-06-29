{-# language NamedFieldPuns #-}

module Quake3.Logic where

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
      anglesFromMouseMotion mouseMotionEvents

    forwardAxis <-
      keyAxis keyboardEvents SDL.ScancodeW SDL.ScancodeS

    rightAxis <-
      keyAxis keyboardEvents SDL.ScancodeD SDL.ScancodeA

    camera pitchAndYaw forwardAxis rightAxis physicsSteps


anglesFromMouseMotion
  :: (MonadMoment m, Fractional a)
  => Event SDL.MouseMotionEventData -> m (Behavior (V2 a))
anglesFromMouseMotion mouseMotionEvents =
  let
    pixelMotionToRadians (V2 x y) =
      negate (V2 (fromIntegral x) (fromIntegral y)) * 0.001

    update mouseMotionEvent old =
      old + pixelMotionToRadians (SDL.mouseMotionEventRelMotion mouseMotionEvent)

  in
    accumB (V2 0 0) (fmap update mouseMotionEvents)


keyAxis
  :: (Num b, MonadMoment m)
  => Event SDL.KeyboardEventData
  -> SDL.Scancode
  -> SDL.Scancode
  -> m (Behavior b)
keyAxis keyboardEvents positive negative =
  let
    filterKeyEvents k =
      filterJust
        (fmap (justScancode k) keyboardEvents)

    keyState =
      stepper SDL.Released . fmap SDL.keyboardEventKeyMotion

    keyStateToSpeed state =
      case state of
        SDL.Pressed ->
          1

        SDL.Released ->
          0

  in
    do
      posState <-
        keyState (filterKeyEvents positive)

      negState <-
        keyState (filterKeyEvents negative)

      return (fmap keyStateToSpeed posState - fmap keyStateToSpeed negState)


camera
  :: (MonadMoment m, RealFloat a, Conjugate a, Epsilon a)
  => Behavior (V2 a) -- ^ The yaw and pitch of the camera
  -> Behavior a -- ^ Motion along the forward axis
  -> Behavior a -- ^ Motion along the right (strafe) axis
  -> Event a -- ^ Physics clock ticks
  -> m (Behavior (M44 a))
camera pitchAndYaw forwardAxis rightAxis physicsSteps =
  do
    let
      -- Form a rotation quaternion from the pitch and yaw angles
      anglesToRotation (V2 x y) =
        axisAngle (V3 0 1 0) x * axisAngle (V3 1 0 0) y

      cameraRotation =
        fmap anglesToRotation pitchAndYaw

      -- The forward vector of the camera in world space
      forwardVector =
        rotate <$> cameraRotation <*> pure (V3 0 0 (-1))

      -- The right (strafe) vector of the camera in world space
      rightVector =
        fmap
          (\(V2 x _) ->
             let
               rotation =
                 axisAngle (V3 0 1 0) (x - pi / 2)

             in
               rotate rotation (V3 0 0 (-1)))
          pitchAndYaw

    let
      forwardMotion =
        100 * forwardAxis

      strafeMotion =
        100 * rightAxis

    cameraPosition <-
      accumB
        (V3 680 100 (-100))
        (let
           update motion dt oldPosition =
             oldPosition + motion ^* dt

         in
           update
             <$> (liftA2 (^*) forwardVector forwardMotion +
                  liftA2 (^*) rightVector strafeMotion)
             <@> physicsSteps)

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


justScancode :: SDL.Scancode
             -> SDL.KeyboardEventData
             -> Maybe SDL.KeyboardEventData
justScancode k e =
  if SDL.keysymScancode (SDL.keyboardEventKeysym e) == k
    then Just e
    else Nothing
