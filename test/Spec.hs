import qualified Data.Earnest.EGraph.Tests      as DEGraph
import qualified Data.Earnest.Exchange.Tests    as DExchange
import qualified Data.Earnest.Transaction.Tests as Transaction
import qualified Earnest.Exchange.Tests         as Exchange
import qualified Earnest.Reactor.Tests          as Reactor
import           Test.Tasty

dataTests :: TestTree
dataTests = testGroup "data modules" [ DExchange.tests
                                     , DEGraph.tests
                                     , Transaction.tests
                                     ]

earnestTests :: TestTree
earnestTests = testGroup "earnest" [ Exchange.tests
                                   , Reactor.tests
                                   ]

tests :: TestTree
tests = testGroup "all tests" [ dataTests
                              , earnestTests
                              ]

main :: IO ()
main = defaultMain tests
