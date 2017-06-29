module Stopwatch where

import Data.IORef
import Control.Monad.IO.Class
import System.Clock

data Stopwatch =
  Stopwatch (IORef TimeSpec)


newStopwatch :: MonadIO m => m Stopwatch
newStopwatch =
  liftIO (fmap Stopwatch (getTime Monotonic >>= newIORef))


lapStopwatch :: MonadIO m => Stopwatch -> m TimeSpec
lapStopwatch (Stopwatch lastTimeRef) =
  do
    t <-
      liftIO (getTime Monotonic)

    t0 <-
      liftIO (readIORef lastTimeRef)

    liftIO (writeIORef lastTimeRef t)

    return (diffTimeSpec t t0)


timeSpecToSeconds :: Fractional a => TimeSpec -> a
timeSpecToSeconds ts =
  fromInteger (toNanoSecs ts) * 1e-9
