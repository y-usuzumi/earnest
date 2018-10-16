module Data.Earnest.EGraph.Neo4j.Tests where

import           Data.Default
import           Data.Text        as T
import qualified Database.Bolt    as B
import           Test.Earnest.Env
import           Test.Tasty
import           Test.Tasty.HUnit

boltDef :: B.BoltCfg
boltDef = def

withNeo4jEnv :: (IO B.Pipe -> TestTree) -> TestTree
withNeo4jEnv t =
  askOption $ \TestEnv{..} -> let
    boltCfg = boltDef { host = host neo4j
                      , port = port neo4j
                      , username = T.pack $ username (neo4j :: Neo4jConfig)
                      , password = T.pack $ password (neo4j :: Neo4jConfig)
                      }
    in t (B.connect boltCfg)

testCreatingNodes :: TestTree
testCreatingNodes = testCase "testCreatingNodes" $ 1 @?= 1
