import qualified Earnest.Exchange.Tests    as Exchange
import qualified Earnest.Transaction.Tests as Transaction
import           Test.Tasty

tests :: TestTree
tests = testGroup "all tests" [ Exchange.tests
                              , Transaction.tests
                              ]

main :: IO ()
main = defaultMain tests
