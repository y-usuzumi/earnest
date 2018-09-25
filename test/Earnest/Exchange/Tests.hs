module Earnest.Exchange.Tests where

import qualified Earnest.Exchange.ServiceProviders.Tests as ServiceProviders
import qualified Earnest.Exchange.TradeInfo.Tests        as TradeInfo
import           Test.Tasty

tests :: TestTree
tests = testGroup "Exchange" [ TradeInfo.tests
                             , ServiceProviders.tests
                             ]
