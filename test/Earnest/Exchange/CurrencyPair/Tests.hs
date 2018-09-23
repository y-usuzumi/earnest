module Earnest.Exchange.CurrencyPair.Tests where

import           Test.Tasty
import           Test.Tasty.HUnit

testSupports :: TestTree
testSupports = testCase "supports" $ do
  undefined

tests :: TestTree
tests = testGroup "exchange" [ testSupports
                             ]
