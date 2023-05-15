{-# LANGUAGE MultiParamTypeClasses #-}

module Core where
import Control.Concurrent.STM (STM)
  
class Has m a where
  has :: m a

class LiftSTM m where
 liftSTM :: STM a -> m a

class Log m where
  logOutput :: Show a => a -> m ()
