module Earnest.Transaction where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class

newtype Transaction r = Transaction { action :: forall n. MonadThrow n => n r
                                    } deriving Functor

instance Applicative Transaction where
  pure a = Transaction $ return a
  (<*>) = ap

instance Monad Transaction where
  Transaction action >>= f = Transaction $ do
    action >>= \r -> do
      let Transaction next = f r
      next

data CreateOrderException deriving Show

instance Exception CreateOrderException
