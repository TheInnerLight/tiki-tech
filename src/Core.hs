{-# LANGUAGE MultiParamTypeClasses #-}

module Core where
import Control.Concurrent.STM (STM)
import Data.Time.Clock.System (SystemTime)
  
class Has m a where
  has :: m a

class LiftSTM m where
 liftSTM :: STM a -> m a

class Log m where
  logOutput :: Show a => a -> m ()

class GetSystemTime m where
  systemTimeNow :: m SystemTime

class Random m where
  randomNormalMeanStd :: Double -> Double -> m Double
