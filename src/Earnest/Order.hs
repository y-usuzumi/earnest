module Earnest.Order where


data Order = Buy Currency Currency Amount
           | Sell Currency Currency Amount
