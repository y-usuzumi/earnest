module Earnest.Exchange.ServiceProviders.Tests where

import           Control.Monad.State
import           Earnest.Exchange
import           Earnest.Exchange.ServiceProviders.AEX
import           Earnest.Exchange.ServiceProviders.GateHub
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit

-- AEX

initAEX :: IO AEXExchange
initAEX = do
  username <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_AEX_USERNAME"
  password <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_AEX_PASSWORD"
  return AEXExchange{ username = username
                    , password = password
                    }

testAEX :: TestTree
testAEX = testCase "AEX" $ do
  aex <- initAEX
  ei <- loadInfo aex
  print ei
  return ()

-- GateHub

initGateHubEnv :: IO GateHubExchange
initGateHubEnv = do
  username <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_GATEHUB_USERNAME"
  password <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_GATEHUB_PASSWORD"
  return GateHubExchange{ username = username
                        , password = password
                        }

testGateHub :: TestTree
testGateHub = testCase "GateHub" $ do
  gateHub <- initGateHubEnv
  ei <- loadInfo gateHub
  return ()

tests :: TestTree
tests = testGroup "ServiceProviders" [ testAEX
                                     ]
