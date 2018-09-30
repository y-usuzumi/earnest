module Data.Earnest.EGraph.Tests where

import           Control.Arrow
import           Data.Hashable
import           Data.Earnest.Currency
import           Data.Earnest.Exchange
import           Data.Earnest.Exchange.TradeInfo
import           Data.Earnest.EGraph
import           GHC.Generics
import           Test.Tasty
import           Test.Tasty.HUnit

data Exchange1 = Exchange1 deriving (Eq, Generic, Ord, Show)
data Exchange2 = Exchange2 deriving (Eq, Generic, Ord, Show)

instance Hashable Exchange1
instance Hashable Exchange2

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
    _supportedTrades = fromList [ (XRP, CNY, TradeInfo { fee = 0.1})
                                ]
                        }

testGraphFromExchanges :: TestTree
testGraphFromExchanges = testCase "graphFromExchanges" $ do
  lxxi <- mapM (\h@(HExchange x) -> loadInfo x >>= \y -> return (h, y)) [ HExchange Exchange1
                                                                        , HExchange Exchange2
                                                                        ]
  let g = graphFromExchanges lxxi
  assertBool "Should support CNY" $ isCurrencySupported CNY g
  assertBool "Should support XRP" $ isCurrencySupported XRP g
  assertBool "Should not support BTS" $ not $ isCurrencySupported BTS g

  length (getTradableOptions BTC g) @?= 2
  length (getTradableOptions CNY g) @?= 1
  length (getTradableOptions XRP g) @?= 1
  length (getTradableOptions BTS g) @?= 0

testExplain :: TestTree
testExplain = testCase "explain" $ do
  lxxi <- mapM (\h@(HExchange x) -> loadInfo x >>= \y -> return (h, y)) [ HExchange Exchange1
                                                                        , HExchange Exchange2
                                                                        ]
  let g = graphFromExchanges lxxi
  explain g

tests :: TestTree
tests = testGroup "EGraph" [ testGraphFromExchanges
                           , testExplain
                           ]
