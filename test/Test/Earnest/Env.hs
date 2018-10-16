module Test.Earnest.Env where

import           Data.Yaml
import           GHC.Generics
import           Options.Applicative
import           System.IO.Unsafe
import           Test.Tasty.Options

data Neo4jConfig = Neo4jConfig { host     :: String
                               , port     :: Int
                               , username :: String
                               , password :: String
                               } deriving (Generic, Show, FromJSON, ToJSON)

data AEXConfig = AEXConfig { username :: String
                           , password :: String
                           } deriving (Generic, Show, FromJSON, ToJSON)

data AEXAPIConfig = AEXAPIConfig { uid  :: String
                                 , key  :: String
                                 , skey :: String
                                 } deriving (Generic, Show, FromJSON, ToJSON)

data GateHubConfig = GateHubConfig { username :: String
                                   , password :: String
                                   } deriving (Generic, Show, FromJSON, ToJSON)

data BourseConfig = AEXBourseConfig AEXConfig
                  | AEXAPIBourseConfig AEXAPIConfig
                  | GateHubBourseConfig GateHubConfig
            deriving (Generic,Show, FromJSON, ToJSON)

data TestEnv = TestEnv { neo4j   :: Neo4jConfig
                       , aex     :: AEXConfig
                       , aexapi  :: AEXAPIConfig
                       , gatehub :: GateHubConfig
                       , bourses :: [BourseConfig]
                       } deriving (Generic, Show, FromJSON, ToJSON)

instance IsOption TestEnv where
  defaultValue = error "MUST apply a config file"
  parseValue fp = Just (unsafePerformIO $ decodeFileThrow fp :: TestEnv)
  optionName = return "config-file"
  optionHelp = return "The config file for running tests (in YAML format)"
  optionCLParser = mkOptionCLParser $ short 'c'

