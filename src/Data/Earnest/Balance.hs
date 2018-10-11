module Data.Earnest.Balance where

import Data.Earnest.Currency
import Data.HashMap.Strict

type Balance = Double
type BalanceTable = HashMap Currency Balance

newBalanceTable :: BalanceTable
newBalanceTable = empty
