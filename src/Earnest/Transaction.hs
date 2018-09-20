module Earnest.Transaction where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class

newtype Transaction r = Transaction { action :: forall m n. (MonadIO m, MonadThrow n) => m (n r)
                                    } deriving Functor

instance Applicative Transaction where
  pure a = Transaction $ return $ return a
  (<*>) = ap

instance Monad Transaction where
  Transaction action >>= f = join $ fmap (join . fmap f) action

instance MonadIO Transaction where
  liftIO io = Transaction $ liftIO $ fmap return io

instance MonadThrow Transaction where
  throwM e = Transaction $ return $ throwM e

data CreateOrderException deriving Show

instance Exception CreateOrderException
