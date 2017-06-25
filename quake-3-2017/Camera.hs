{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera where

import Control.Lens
import Data.Monoid (First)
import Linear
import Reactive.Banana

data UpdateCamera a
  = WalkCamera a
  | StrafeCamera a
  | RotateCamera (V2 a)
  deriving (Eq,Functor,Show)

makePrisms ''UpdateCamera

cameraViewMatrix
  :: (Conjugate a, Epsilon a, MonadMoment m, RealFloat a)
  => V3 a -> Event (UpdateCamera a) -> m (Behavior (M44 a))
cameraViewMatrix origin updateCamera = do
  let walked =
        filterPrism _WalkCamera updateCamera
      strafed =
        filterPrism _StrafeCamera updateCamera
      rotated =
        filterPrism _RotateCamera updateCamera
      forwardVector = do
        velocityVector <- sumEvents 0 rotated
        pure $
          fmap (\(V2 x y) ->
                  rotate (axisAngle (V3 0 1 0) x *
                          axisAngle (V3 1 0 0) y)
                        (V3 0 0 (-1)))
              velocityVector

  forward <- forwardVector

  let rightVector = do
        fmap (rotate (axisAngle (V3 0 1 0)
                                (pi / 2)))
             forward
  cameraPosition <-
        sumEvents origin
                  (unionWith (+)
                         ((^*) <$> forward <@> walked)
                         ((^*) <$>
                          (rightVector & mapped . _y .~ 0) <@>
                          strafed))
  pure $
    liftA3 lookAt
           cameraPosition
           (liftA2 (+) cameraPosition forward)
           (pure (V3 0 1 0))


filterPrism :: Getting (First b) a b -> Event a -> Event b
filterPrism p = filterJust . fmap (preview p)


sumEvents
  :: (MonadMoment m, Num a)
  => a -> Event a -> m (Behavior a)
sumEvents a e = accumB a (fmap (+) e)
