module Earnest.Bourse.Tests where

import qualified Earnest.Bourse.Providers.Tests as Providers
import           Test.Earnest.Env
import           Test.Tasty

tests :: TestEnv -> TestTree
tests env = testGroup "Bourse" [ Providers.tests env
                               ]
