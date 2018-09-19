module Earnest.Order where

import Earnest.Currency


type Amount = Double

data Order = Buy Currency Currency Amount
           | Sell Currency Currency Amount
