import           Data.ByteString.Char8          as C8
import qualified Data.Earnest.Bourse.Tests      as DBourse
import qualified Data.Earnest.EGraph.Tests      as DEGraph
import qualified Data.Earnest.Transaction.Tests as Transaction
import           Data.Yaml                      hiding (Parser)
import qualified Earnest.Bourse.Tests           as Bourse
import qualified Earnest.Reactor.Tests          as Reactor
import           Options.Applicative
import           Test.Earnest.Env
import           Test.Tasty

dataTests :: TestEnv -> TestTree
dataTests env = testGroup "data modules" [ DBourse.tests env
                                         , DEGraph.tests env
                                         , Transaction.tests env
                                         ]

earnestTests :: TestEnv -> TestTree
earnestTests env = testGroup "earnest" [ Bourse.tests env
                                       , Reactor.tests env
                                       ]

tests :: TestEnv -> TestTree
tests env = testGroup "all tests" [ dataTests env
                                  , earnestTests env
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
  Args{..} <- execParser opts
  eitherConfig <- decodeFileEither configFile
  let Right cfg = eitherConfig
  defaultMain $ tests $ TestEnv { earnestConfig = cfg }
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "Run tests"
      )
