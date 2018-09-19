module Earnest.Transaction.Tests where

import Test.Tasty
import Test.Tasty.HUnit

testCreateOrder :: TestTree
testCreateOrder = testCase "createOrder" $ do
  1 @=? 1


tests :: TestTree
tests = testGroup "transaction" [ testCreateOrder
                                ]
