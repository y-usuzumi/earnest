module Data.Earnest.Transaction.Tests where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Earnest.Transaction
import           Test.Earnest.Env
import           Test.Tasty
import           Test.Tasty.HUnit

testCreateOrder :: TestTree
testCreateOrder = testCase "createOrder" $
  runTransaction $
    liftIO $ putStrLn "GG"

tests :: TestTree
tests = testGroup "Transaction" [ testCreateOrder
                                ]
