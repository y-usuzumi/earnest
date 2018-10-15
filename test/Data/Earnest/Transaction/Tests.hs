module Data.Earnest.Transaction.Tests where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Earnest.Transaction
import           Test.Earnest.Env
import           Test.Tasty
import           Test.Tasty.HUnit

testCreateOrder :: TestEnv -> TestTree
testCreateOrder env = testCase "createOrder" $
  runTransaction $
    liftIO $ putStrLn "GG"

tests :: TestEnv -> TestTree
tests env = testGroup "Transaction" [ testCreateOrder env
                                    ]
