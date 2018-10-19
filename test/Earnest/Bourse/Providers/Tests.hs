module Earnest.Bourse.Providers.Tests where

import           Control.Lens
import           Control.Monad.State
import           Data.Earnest.Bourse
import           Data.Earnest.EGraph
import           Data.Earnest.EGraph.FGL
import           Data.Earnest.EGraph.Neo4j
import           Data.Graph.Inductive
import           Data.Maybe
import           Earnest.Bourse.Providers.AEX
import           Earnest.Bourse.Providers.AEXAPI
import           Earnest.Bourse.Providers.GateHub
import           Earnest.Config
import           System.Environment
import           Test.Earnest.Env
import           Test.Earnest.Utils               as U
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf


-- AEX

initAEX :: AEXConfig -> IO AEXBourse
initAEX AEXConfig{..} = do
  return AEXBourse{ username = username
                  , password = password
                  }

testAEX :: TestTree
testAEX = testCaseRC "aex" $ withRC env $ \env -> liftIO $ do
  aex <- initAEX $ aex env
  bi <- loadInfo aex
  print bi

-- AEXAPI

initAEXAPI :: AEXAPIConfig -> IO AEXAPIBourse
initAEXAPI AEXAPIConfig{..} = do
  return AEXAPIBourse{ uid = uid
                     , key = key
                     , skey = skey
                     }

testAEXAPI :: TestTree
testAEXAPI = testCaseRC "AEXAPI" $ withRC env $ \env -> liftIO $ do
  aexapi <- initAEXAPI $ aexapi env
  bi <- loadInfo aexapi
  (g :: FGLGraph) <- graphFromBourses () [HBourse aexapi]
  forM_ (labEdges g) $ \(n1, n2, ei) ->
    printf "%s -> %s\n" (show $ fromJust (lab g n1) ^. currency) (show $ fromJust (lab g n2) ^. currency)

testAEXAPINeo4j :: TestTree
testAEXAPINeo4j = testCaseRC "AEXAPINeo4j" $ withRC (env U.& U.neo4j) $ \(env, p) -> liftIO $ do
  aexapi <- initAEXAPI $ aexapi env
  bi <- loadInfo aexapi
  (g :: Neo4jGraph) <- graphFromBourses p [HBourse aexapi]
  print g

-- GateHub

initGateHub :: GateHubConfig -> IO GateHubBourse
initGateHub GateHubConfig{..} = do
  return GateHubBourse{ username = username
                      , password = password
                      }

testGateHub :: TestTree
testGateHub = testCaseRC "GateHub" $ withRC env $ \env -> liftIO $ do
  gateHub <- initGateHub $ gatehub env
  bi <- loadInfo gateHub
  return ()

tests :: TestTree
tests = testGroup "Providers" [ testAEX
                              , testAEXAPI
                              , testAEXAPINeo4j
                              , testGateHub
                              ]
