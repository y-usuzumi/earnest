import           Data.ByteString.Char8          as C8
import qualified Data.Earnest.Bourse.Tests      as DBourse
import qualified Data.Earnest.EGraph.Tests      as DEGraph
import qualified Data.Earnest.Transaction.Tests as Transaction
import           Data.Proxy
import           Data.Yaml                      hiding (Parser)
import qualified Earnest.Bourse.Tests           as Bourse
import           Earnest.Config
import qualified Earnest.Reactor.Tests          as Reactor
import           Options.Applicative
import           Test.Earnest.Env
import           Test.Tasty
import           Test.Tasty.Options

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
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = configFileIngredient:defaultIngredients
    configFileIngredient = includingOptions [Option (Proxy :: Proxy TestEnv)]
