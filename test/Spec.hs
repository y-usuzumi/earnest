import qualified Earnest.Transaction.Tests as T
import           Test.Tasty

tests :: TestTree
tests = testGroup "all tests" [ T.tests
                              ]

main :: IO ()
main = defaultMain tests
