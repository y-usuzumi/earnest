import qualified Data.Earnest.Bourse.Tests      as DBourse
import qualified Data.Earnest.EGraph.Tests      as DEGraph
import qualified Data.Earnest.Transaction.Tests as Transaction
import qualified Earnest.Bourse.Tests           as Bourse
import qualified Earnest.Reactor.Tests          as Reactor
import           Test.Tasty

dataTests :: TestTree
dataTests = testGroup "data modules" [ DBourse.tests
                                     , DEGraph.tests
                                     , Transaction.tests
                                     ]

earnestTests :: TestTree
earnestTests = testGroup "earnest" [ Bourse.tests
                                   , Reactor.tests
                                   ]

tests :: TestTree
tests = testGroup "all tests" [ dataTests
                              , earnestTests
                              ]

main :: IO ()
main = defaultMain tests
