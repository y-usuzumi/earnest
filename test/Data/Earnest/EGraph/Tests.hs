module Data.Earnest.EGraph.Tests where

import           Data.Earnest.EGraph.FGL.Tests as FGL
import           Data.Earnest.EGraph.Neo4j.Tests as Neo4j
import           Test.Earnest.Env
import           Test.Tasty

tests :: TestTree
tests = testGroup "EGraph" [ FGL.tests
                           , Neo4j.tests
                           ]
