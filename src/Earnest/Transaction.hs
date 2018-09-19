module Earnest.Transaction where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class

newtype Transaction n r = Transaction { action :: MonadThrow n => n r
                                      } deriving Functor

instance Monad n => Applicative (Transaction n) where
  pure a = Transaction $ return a
  (<*>) = ap

instance Monad n => Monad (Transaction n) where
  Transaction action >>= f = Transaction $ do
    action >>= \r -> do
      let Transaction next = f r
      next

data CreateOrderException deriving Show

instance Exception CreateOrderException
