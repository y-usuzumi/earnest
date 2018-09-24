module Earnest.Exchange.ServiceProviders.Tests where

import           Control.Monad.State
import           Earnest.Exchange
import           Earnest.Exchange.ServiceProviders.AEX
import           Earnest.Exchange.ServiceProviders.GateHub
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit

-- AEX

initAEXEnv :: IO AEXExchangeEnv
initAEXEnv = do
  username <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_AEX_USERNAME"
  password <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_AEX_PASSWORD"
  return AEXExchangeEnv{ username = username
                       , password = password
                       }

testAEX :: TestTree
testAEX = testCase "AEX" $ do
  env <- initAEXEnv
  ei <- loadInitialInfo AEXExchange env
  return ()

-- GateHub

initGateHubEnv :: IO GateHubExchangeEnv
initGateHubEnv = do
  username <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_GATEHUB_USERNAME"
  password <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_GATEHUB_PASSWORD"
  return GateHubExchangeEnv{ username = username
                           , password = password
                           }

testGateHub :: TestTree
testGateHub = testCase "GateHub" $ do
  env <- initGateHubEnv
  ei <- loadInitialInfo GateHubExchange env
  return ()

tests :: TestTree
tests = testGroup "ServiceProviders" [ testAEX
                                     ]
