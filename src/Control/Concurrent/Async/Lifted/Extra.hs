{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.Async.Lifted.Extra
  ( mapPool
  , forPool
  )

where

import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Control
import Control.Concurrent.QSem.Lifted
import Control.Exception.Lifted
import Control.Monad.IO.Class

-- http://stackoverflow.com/questions/18896103/can-haskells-control-concurrent-async-mapconcurrently-have-a-limit

mapPool
  :: (MonadIO m, MonadBaseControl IO m)
  => Traversable t =>
       Int -> (a -> m b) -> t a -> m (t b)
mapPool n f xs = do
    sem <- newQSem n
    mapConcurrently (with sem . f) xs

forPool
  :: (Traversable t, MonadBaseControl IO m, MonadIO m)
  => Int -> t a -> (a -> m b) -> m (t b)
forPool n = flip $ mapPool n

with
  :: MonadBaseControl IO m
  => QSem -> m c -> m c
with m =  bracket_ (waitQSem m) (signalQSem m)
