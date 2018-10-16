module Earnest.Bourse.Providers.Tests where

import           Control.Lens
import           Control.Monad.State
import           Data.Earnest.Bourse
import           Data.Earnest.EGraph
import           Data.Earnest.EGraph.FGL
import           Data.Graph.Inductive
import           Data.Maybe
import           Earnest.Bourse.Providers.AEX
import           Earnest.Bourse.Providers.AEXAPI
import           Earnest.Bourse.Providers.GateHub
import           Earnest.Config
import           System.Environment
import           Test.Earnest.Env
import           Test.Earnest.Utils.TH
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf


-- AEX

initAEX :: Config -> IO AEXBourse
initAEX cfg = do
  let AEXBourseConfig{..} = $(findBourseConfig 'cfg 'AEXBourseConfig)
  return AEXBourse{ username = username
                  , password = password
                  }

testAEX :: TestTree
testAEX = askOption $ \env -> testCase "AEX" $ do
  let cfg = earnestConfig env
  aex <- initAEX cfg
  bi <- loadInfo aex
  print bi

-- AEXAPI

initAEXAPI :: Config -> IO AEXAPIBourse
initAEXAPI cfg = do
  let AEXAPIBourseConfig{..} = $(findBourseConfig 'cfg 'AEXAPIBourseConfig)
  return AEXAPIBourse{ uid = uid
                     , key = key
                     , skey = skey
                     }

testAEXAPI :: TestTree
testAEXAPI = askOption $ \env -> testCase "AEXAPI" $ do
  let cfg = earnestConfig env
  aexapi <- initAEXAPI cfg
  bi <- loadInfo aexapi
  (g :: FGLGraph) <- graphFromBourses [HBourse aexapi]
  forM_ (labEdges g) $ \(n1, n2, ei) ->
    printf "%s -> %s\n" (show $ fromJust (lab g n1) ^. currency) (show $ fromJust (lab g n2) ^. currency)

-- GateHub

initGateHub :: Config -> IO GateHubBourse
initGateHub cfg = do
  let GateHubBourseConfig{..} = $(findBourseConfig 'cfg 'GateHubBourseConfig)
  return GateHubBourse{ username = username
                      , password = password
                      }

testGateHub :: TestTree
testGateHub = askOption $ \env -> testCase "GateHub" $ do
  let cfg = earnestConfig env
  gateHub <- initGateHub cfg
  bi <- loadInfo gateHub
  return ()

tests :: TestTree
tests = testGroup "Providers" [ testAEX
                              , testAEXAPI
                              , testGateHub
                              ]
