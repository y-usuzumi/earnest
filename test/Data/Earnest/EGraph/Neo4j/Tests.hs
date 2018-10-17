module Data.Earnest.EGraph.Neo4j.Tests where

import           Data.Default
import           Data.Text          as T
import qualified Database.Bolt      as B
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
      B.connect boltCfg
    finalizer = B.close

testCreatingNodes :: TestTree
testCreatingNodes = withNeo4jEnv "creatingNodes" $ \p -> do
  1 @?= 1

tests :: TestTree
tests = testGroup "Neo4j" [ testCreatingNodes
                          ]
