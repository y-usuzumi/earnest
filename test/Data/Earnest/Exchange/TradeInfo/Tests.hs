module Data.Earnest.Exchange.TradeInfo.Tests where

import           Control.Monad.State
import           Data.Earnest.Currency
import           Data.Earnest.Exchange.TradeInfo
import           Test.Tasty
import           Test.Tasty.HUnit

testSupports :: TestTree
testSupports = testCase "isSupported" $ do
  let newLookup = flip execState newTradeInfoLookup $ do
        merge [ (CNY, XRP, undefined)
              , (CNY, BTC, undefined)
              , (BTC, XRP, undefined)
              , (BTC, CNY, undefined)
              ]
  assertBool "Should support CNY -> XRP" (isSupported (CNY, XRP) newLookup)
  assertBool "Should support CNY -> BTC" (isSupported (CNY, BTC) newLookup)
  assertBool "Should support BTC -> XRP" (isSupported (BTC, XRP) newLookup)
  assertBool "Should support BTC -> CNY" (isSupported (BTC, CNY) newLookup)
  assertBool "Should not support CNY -> CNY" (not $ isSupported (CNY, CNY) newLookup)
  assertBool "Should not support XRP -> CNY" (not $ isSupported (XRP, CNY) newLookup)

tests :: TestTree
tests = testGroup "CurrencyPair" [ testSupports
                                 ]
