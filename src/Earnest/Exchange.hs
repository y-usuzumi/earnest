module Earnest.Exchange where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Earnest.Order
import           Earnest.Transaction


class Exchange e where
  createOrder :: (MonadIO m, MonadThrow n) => e -> Order -> Transaction m n ()
