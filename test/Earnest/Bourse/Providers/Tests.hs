module Earnest.Bourse.Providers.Tests where

import           Control.Lens
import           Control.Monad.State
import           Data.Earnest.Bourse
import           Data.Earnest.EGraph.FGL
import           Data.Graph.Inductive
import           Data.Maybe
import           Earnest.Bourse.Providers.AEX
import           Earnest.Bourse.Providers.AEXAPI
import           Earnest.Bourse.Providers.GateHub
import           System.Environment
import           Test.Earnest.Util
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf
import Earnest.Config
import Test.Earnest.Env

-- AEX

initAEX :: TestEnv -> IO AEXBourse
initAEX env = do
  let cfg = earnestConfig env
  let AEXBourseConfig{..} = $(findBourseConfig 'cfg 'AEXBourseConfig)
  return AEXBourse{ username = username
                  , password = password
                  }

testAEX :: TestEnv -> TestTree
testAEX env = testCase "AEX" $ do
  aex <- initAEX env
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

testAEXAPI :: TestEnv -> TestTree
testAEXAPI env = testCase "AEXAPI" $ do
  aexapi <- initAEXAPI
  bi <- loadInfo aexapi
  g <- graphFromBourses [HBourse aexapi]
  forM_ (labEdges g) $ \(n1, n2, ei) -> do
    printf "%s -> %s\n" (show $ fromJust (lab g n1) ^. currency) (show $ fromJust (lab g n2) ^. currency)
  return ()

-- GateHub

initGateHub :: IO GateHubBourse
initGateHub = do
  username <- liftIO $ getEnv "EARNEST_PROVIDER_GATEHUB_USERNAME"
  password <- liftIO $ getEnv "EARNEST_PROVIDER_GATEHUB_PASSWORD"
  return GateHubBourse{ username = username
                      , password = password
                      }

testGateHub :: TestEnv -> TestTree
testGateHub env = testCase "GateHub" $ do
  gateHub <- initGateHub
  bi <- loadInfo gateHub
  return ()

tests :: TestEnv -> TestTree
tests env = testGroup "Providers" [ testAEX env
                                  , testAEXAPI env
                                  , testGateHub env
                                  ]
