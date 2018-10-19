module Test.Earnest.Utils where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Default
import           Data.Text              as T
import qualified Database.Bolt          as B
import           Test.Earnest.Env
import           Test.Tasty
import           Test.Tasty.HUnit

type RCAssertion = ReaderT TestEnv IO ()

data ResourceControl a = RC { initialize :: TestEnv -> IO a
                            , finalize   :: a -> IO ()
                            }

infix 9 &
(&) :: ResourceControl a -> ResourceControl b -> ResourceControl (a, b)
rcA & rcB = let
  i env = (,) <$> initialize rcA env <*> initialize rcB env
  f (a, b) = finalize rcB b `finally` finalize rcA a
  in RC { initialize = i
        , finalize = f
        }

runRC :: ResourceControl a -> TestEnv -> (a -> Assertion) -> Assertion
runRC RC{..} env assertion = do
  res <- initialize env
  assertion res `finally` finalize res

neo4j :: ResourceControl B.Pipe
neo4j = RC { initialize = i
           , finalize = f
           }
  where
    i TestEnv{..} = do
      let boltCfg = def { B.host = host neo4j
                        , B.port = port neo4j
                        , B.user = T.pack $ username (neo4j :: Neo4jConfig)
                        , B.password = T.pack $ password (neo4j :: Neo4jConfig)
                        }
      p <- B.connect boltCfg
      B.run p $ B.query "match (n) delete n"
      return p
    f p = do
      B.run p $ B.query "match (n) delete n"
      B.close p

withRC :: ResourceControl a -> (a -> RCAssertion) -> RCAssertion
withRC a assertion = do
  env <- ask
  liftIO $ runRC a env $ \res -> runReaderT (assertion res) env

testCaseRC :: TestName -> RCAssertion -> TestTree
testCaseRC name r = askOption $ \env -> testCase name $ runReaderT r env
