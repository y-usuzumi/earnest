module Earnest.Transaction where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class

newtype Transaction m n r = Transaction { action :: (MonadIO m, MonadThrow n) => m (n r)
                                        } deriving Functor

instance Applicative (Transaction m n) where
  pure a = Transaction $ return $ return a
  (<*>) = ap

instance Monad (Transaction m n) where
  (>>=) = undefined

data CreateOrderException deriving Show

instance Exception CreateOrderException
