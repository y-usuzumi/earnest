module Earnest.Transaction.Tests where

import Earnest.Transaction
import           Control.Monad.Catch
import           Test.Tasty
import           Test.Tasty.HUnit

testCreateOrder :: TestTree
testCreateOrder = testCase "createOrder" $ do
  let t = Transaction action
  undefined
  where
    action :: IO ()
    action = putStrLn "GG"


tests :: TestTree
tests = testGroup "transaction" [ testCreateOrder
                                ]
