module Earnest.Exchange.ServiceProviders.Tests where

import           Control.Monad.State
import           Earnest.Exchange
import           Earnest.Exchange.ServiceProviders.AEX
import           Test.Tasty
import           Test.Tasty.HUnit

testAEX :: TestTree
testAEX = testCase "AEX" $ do
  ei <- loadInitialInfo AEXExchange
  return ()

tests :: TestTree
tests = testGroup "ServiceProviders" [ testAEX
                                     ]
