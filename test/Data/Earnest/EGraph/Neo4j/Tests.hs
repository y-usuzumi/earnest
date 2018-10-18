module Data.Earnest.EGraph.Neo4j.Tests where

import           Data.Default
import           Data.Earnest.EGraph
import           Data.Earnest.EGraph.Neo4j
import           Data.Earnest.TestData
import           Data.Earnest.TestData.TH
import           Data.Text                 as T
import qualified Database.Bolt             as B
import           Test.Earnest.Env
import           Test.Earnest.Utils
import           Test.Tasty
import           Test.Tasty.HUnit

withNeo4jEnv :: TestName -> (B.Pipe -> Assertion) -> TestTree
withNeo4jEnv testName assertion =
  testWithResource testName initializer assertion finalizer
  where
    initializer TestEnv{..} = do
      let boltCfg = def { B.host = host neo4j
                        , B.port = port neo4j
                        , B.user = T.pack $ username (neo4j :: Neo4jConfig)
                        , B.password = T.pack $ password (neo4j :: Neo4jConfig)
                        }
      p <- B.connect boltCfg
      B.run p $ B.query "match (n) delete n"
      return p
    finalizer p = do
      B.run p $ B.query "match (n) delete n"
      B.close p

testGraphFromBourses :: TestTree
testGraphFromBourses = withNeo4jEnv "creatingNodes" $ \p -> do
  let bs = $(boursesFromNames [ [|Bourse1|]
                              , [|Bourse2|]
                              ])
  (g :: Neo4jGraph) <- graphFromBourses p bs
  print g

tests :: TestTree
tests = testGroup "Neo4j" [ testGraphFromBourses
                          ]
