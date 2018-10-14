import           Data.ByteString.Char8          as C8
import qualified Data.Earnest.Bourse.Tests      as DBourse
import qualified Data.Earnest.EGraph.Tests      as DEGraph
import qualified Data.Earnest.Transaction.Tests as Transaction
import           Data.Yaml                      hiding (Parser)
import qualified Earnest.Bourse.Tests           as Bourse
import qualified Earnest.Reactor.Tests          as Reactor
import           Options.Applicative
import           Test.Earnest
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

data Args = Args { configFile :: String
                 }

argsParser :: Parser Args
argsParser = Args
  <$> argument str ( metavar "CONFIGFILE"
                   <> help "The config file for running tests (in YAML format)"
                   )

main :: IO ()
main = do
  args <- execParser opts
  let cfg = Config {
        bourses = [ AEXBourseConfig { username = "SDF"
                                    , password = "GGG"
                                    }
                  , AEXAPIBourseConfig { uid = "UID"
                                       , key = "KEY"
                                       , skey = "SKEY"
                                       }
                  , GateHubBourseConfig { username = "X"
                                        , password = "Y"
                                        }
                  ]
        }
  C8.putStrLn $ encode cfg
  -- defaultMain tests
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "Run tests"
      )
