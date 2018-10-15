module Data.Earnest.EGraph.Tests where

import           Data.Earnest.EGraph.FGL.Tests as FGL
import           Test.Earnest.Env
import           Test.Tasty

tests :: TestEnv -> TestTree
tests env = testGroup "EGraph" [ FGL.tests env
                               ]
