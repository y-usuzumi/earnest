module Data.Earnest.TestData where

import           Control.Monad
import           Data.Earnest.Currency
import           Data.Earnest.Exchange
import           Data.Earnest.Exchange.TradeInfo
import           Data.Hashable
import           GHC.Generics


data Exchange1 = Exchange1 deriving (Eq, Generic, Ord, Show)
data Exchange2 = Exchange2 deriving (Eq, Generic, Ord, Show)
data Exchange3 = Exchange3 deriving (Eq, Generic, Ord, Show)

instance Hashable Exchange1
instance Hashable Exchange2
instance Hashable Exchange3

instance Exchange Exchange1 where
  loadInfo _ =
    return ExchangeInfo {
    _supportedTrades = fromList [ (BTC, CNY, TradeInfo { fee = 0.02 })
                                , (CNY, BTC, TradeInfo { fee = 0.01 })
                                , (BTC, XRP, TradeInfo { fee = 0.05 })
                                ]
                        }

instance Exchange Exchange2 where
  loadInfo _ =
    return ExchangeInfo {
    _supportedTrades = fromList [ (XRP, CNY, TradeInfo { fee = 0.1 })
                                ]
                        }

instance Exchange Exchange3 where
  loadInfo _ =
    return ExchangeInfo {
    _supportedTrades = fromList [ (XLM, NXT, TradeInfo { fee = 0.1 })
                                ]
                        }
