module Earnest.Exchange.ServiceProviders.Tests where

import           Control.Monad.State
import           Earnest.Exchange
import           Earnest.Exchange.ServiceProviders.AEX
import           Earnest.Exchange.ServiceProviders.GateHub
import           Test.Tasty
import           Test.Tasty.HUnit

testGateHub :: TestTree
testGateHub = testCase "GateHub" $ do
  ei <- loadInitialInfo GateHubExchange
  return ()

testAEX :: TestTree
testAEX = testCase "AEX" $ do
  ei <- loadInitialInfo AEXExchange
  return ()


tests :: TestTree
tests = testGroup "ServiceProviders" [ testAEX
                                     ]
