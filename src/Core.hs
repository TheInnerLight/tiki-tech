{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

module Core where
import Control.Concurrent.STM (STM)
import Data.Time.Clock.System (SystemTime)
import GHC.Base (Symbol)
  
class Has m a where
  has :: m a

class Atomise m where
  atomise :: STM a -> m a

class Log m where
  logOutput :: Show a => a -> m ()

class GetSystemTime m where
  systemTimeNow :: m SystemTime

class Random m where
  randomNormalMeanStd :: Double -> Double -> m Double

class Concurrent m where
  mapConcurrently :: Traversable t => (a -> m b) -> t a -> m (t b)

class (CacheKeyValue l) => Cache m (l :: Symbol) where
  cacheLookup :: CacheKey l -> m (Maybe (CacheValue l))
  cacheInsert :: CacheKey l -> CacheValue l -> m ()

class CacheKeyValue (l :: Symbol) where
  type CacheKey l = ck | ck -> l
  type CacheValue l = cv | cv -> l

instance CacheKeyValue "centre-of-play" where
  type CacheKey "centre-of-play" = ()
  type CacheValue "centre-of-play" = (Double, Double)

cached :: (CacheKeyValue l, Cache m (l :: Symbol), Monad m) => (CacheKey l -> m (CacheValue l)) -> CacheKey l -> m (CacheValue l)
cached f k = do
  cl <- cacheLookup k
  case cl of 
    Just res -> pure res
    Nothing  -> do
      res <- f k
      cacheInsert k res
      pure res
