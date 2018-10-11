module Earnest.Bourse.Providers.Tests where

import           Control.Monad.State
import           Data.Earnest.Bourse
import           Earnest.Bourse.Providers.AEX
import           Earnest.Bourse.Providers.AEXAPI
import           Earnest.Bourse.Providers.GateHub
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit

-- AEX

initAEX :: IO AEXBourse
initAEX = do
  username <- liftIO $ getEnv "EARNEST_PROVIDER_AEX_USERNAME"
  password <- liftIO $ getEnv "EARNEST_PROVIDER_AEX_PASSWORD"
  return AEXBourse{ username = username
                  , password = password
                  }

testAEX :: TestTree
testAEX = testCase "AEX" $ do
  aex <- initAEX
  bi <- loadInfo aex
  print bi
  return ()

-- AEXAPI

initAEXAPI :: IO AEXAPIBourse
initAEXAPI = do
  uid <- liftIO $ getEnv "EARNEST_PROVIDER_AEXAPI_UID"
  key <- liftIO $ getEnv "EARNEST_PROVIDER_AEXAPI_KEY"
  skey <- liftIO $ getEnv "EARNEST_PROVIDER_AEXAPI_SKEY"
  return AEXAPIBourse{ uid = uid
                     , key = key
                     , skey = skey
                     }

testAEXAPI :: TestTree
testAEXAPI = testCase "AEXAPI" $ do
  aexapi <- initAEXAPI
  bi <- loadInfo aexapi
  print bi
  return ()

-- GateHub

initGateHubEnv :: IO GateHubBourse
initGateHubEnv = do
  username <- liftIO $ getEnv "EARNEST_PROVIDER_GATEHUB_USERNAME"
  password <- liftIO $ getEnv "EARNEST_PROVIDER_GATEHUB_PASSWORD"
  return GateHubBourse{ username = username
                      , password = password
                      }

testGateHub :: TestTree
testGateHub = testCase "GateHub" $ do
  gateHub <- initGateHubEnv
  bi <- loadInfo gateHub
  return ()

tests :: TestTree
tests = testGroup "Providers" [ testAEX
                              , testAEXAPI
                              , testGateHub
                              ]
