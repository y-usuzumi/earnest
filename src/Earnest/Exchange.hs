module Earnest.Exchange where

import Earnest.Order


class Exchange e where
  createOrder :: Order -> Transaction ()
