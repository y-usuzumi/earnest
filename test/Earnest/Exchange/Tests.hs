module Earnest.Exchange.Tests where

import qualified Earnest.Exchange.ServiceProviders.Tests as ServiceProviders
import           Test.Tasty

tests :: TestTree
tests = testGroup "Exchange" [ ServiceProviders.tests
                             ]
