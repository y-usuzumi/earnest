module Earnest.Exchange where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Earnest.Order
import           Earnest.Transaction


class Exchange e where
  createOrder :: e -> Order -> Transaction ()
