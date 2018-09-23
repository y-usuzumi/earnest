module Earnest.Currency where

import GHC.Generics (Generic)
import           Data.Hashable

data Currency = CNY
              | USD
              | BTC
              | XRP
              deriving (Eq, Generic, Show)

instance Hashable Currency
