module Data.Earnest.EGraph.Tests where

import           Data.Earnest.EGraph.FGL.Tests    as FGL
import           Data.Earnest.EGraph.Simple.Tests as Simple
import           Test.Tasty

tests :: TestTree
tests = testGroup "EGraph" [ FGL.tests
                           , Simple.tests
                           ]
