module Earnest.Transaction where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift

newtype Transaction r = Transaction { runTransaction :: forall m. (MonadIO m, MonadThrow m) => m r
                                    } deriving Functor

instance Applicative Transaction where
  pure a = Transaction $ return a
  (<*>) = ap

instance Monad Transaction where
  Transaction t >>= f = Transaction $ t >>= \r ->
    let Transaction next = f r in next

instance MonadIO Transaction where
  liftIO io = Transaction $ liftIO io

instance MonadThrow Transaction where
  throwM e = Transaction $ throwM e
