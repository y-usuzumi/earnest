module Earnest.Exchange where

import           Earnest.Order
import           Earnest.Transaction


class Exchange e where
  createOrder :: e -> Order -> Transaction ()
