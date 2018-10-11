module Data.Earnest.TestData where

import           Control.Monad
import           Data.Earnest.Balance
import           Data.Earnest.Bourse
import           Data.Earnest.Currency
import           Data.Earnest.TradeInfo
import           Data.Hashable
import           GHC.Generics


data Bourse1 = Bourse1 deriving (Eq, Generic, Ord, Show)
data Bourse2 = Bourse2 deriving (Eq, Generic, Ord, Show)
data Bourse3 = Bourse3 deriving (Eq, Generic, Ord, Show)

instance Hashable Bourse1
instance Hashable Bourse2
instance Hashable Bourse3

instance Bourse Bourse1 where
  loadInfo _ =
    return BourseInfo
    {
      _supportedTrades = fromList [ (BTC, CNY, TradeInfo { fee = 0.02 })
                                  , (CNY, BTC, TradeInfo { fee = 0.01 })
                                  , (BTC, XRP, TradeInfo { fee = 0.05 })
                                  ]
    , _balances = newBalanceTable
    , _confidence = 1
    }

instance Bourse Bourse2 where
  loadInfo _ =
    return BourseInfo
    {
      _supportedTrades = fromList [ (XRP, CNY, TradeInfo { fee = 0.1 })
                                  ]
    , _balances = newBalanceTable
    , _confidence = 1
    }

instance Bourse Bourse3 where
  loadInfo _ =
    return BourseInfo
    {
      _supportedTrades = fromList [ (XLM, NXT, TradeInfo { fee = 0.1 })
                                  ]
    , _balances = newBalanceTable
    , _confidence = 1
    }
