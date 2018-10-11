module Earnest.Bourse.Tests where

import qualified Earnest.Bourse.Providers.Tests as Providers
import           Test.Tasty

tests :: TestTree
tests = testGroup "Bourse" [ Providers.tests
                           ]
