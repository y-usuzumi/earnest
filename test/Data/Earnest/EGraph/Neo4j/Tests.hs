module Data.Earnest.EGraph.Neo4j.Tests where

import           Control.Monad.IO.Class
import           Data.Default
import           Data.Earnest.EGraph
import           Data.Earnest.EGraph.Neo4j
import           Data.Earnest.TestData
import           Data.Earnest.TestData.TH
import           Data.Text                as T
import qualified Database.Bolt            as B
import           Test.Earnest.Utils
import           Test.Tasty
import           Test.Tasty.HUnit

testGraphFromBourses :: TestTree
testGraphFromBourses = testCaseRC "graphFromBourses" $ do
  withRC neo4j $ \p -> do
    let bs = $(boursesFromNames [ [|Bourse1|]
                                , [|Bourse2|]
                                ])
    (g :: Neo4jGraph) <- graphFromBourses p bs
    liftIO $ 1 @?= 1

tests :: TestTree
tests = testGroup "Neo4j" [ testGraphFromBourses
                          ]
