module Test.Earnest.Env where

import           Data.Aeson.TH
import           Data.Yaml
import           GHC.Generics
import           Options.Applicative hiding (Parser)
import           System.IO.Unsafe
import           Test.Tasty.Options
import qualified Data.HashMap.Strict as HM

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
                  deriving (Generic,Show)

instance FromJSON BourseConfig where
  parseJSON = withObject "BourseConfig" $ \v -> do
    typ <- (v .: "type" :: Parser String)
    let vs = Object (HM.delete "type" v)
    case typ of
      "aex" -> AEXBourseConfig <$> parseJSON vs
      "aexapi" -> AEXAPIBourseConfig <$> parseJSON vs
      "gatehub" -> GateHubBourseConfig <$> parseJSON vs

instance ToJSON BourseConfig where
  toJSON (AEXBourseConfig cfg) = let
    Object obj = toJSON cfg
    in Object (HM.insert "type" "aex" obj)
  toJSON (AEXAPIBourseConfig cfg) = let
    Object obj = toJSON cfg
    in Object (HM.insert "type" "aexapi" obj)
  toJSON (GateHubBourseConfig cfg) = let
    Object obj = toJSON cfg
    in Object (HM.insert "type" "gatehub" obj)

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

