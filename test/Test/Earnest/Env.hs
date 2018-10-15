module Test.Earnest.Env where

import           Data.Yaml
import           Earnest.Config
import           GHC.Generics
import           Options.Applicative
import           System.IO.Unsafe
import           Test.Tasty.Options

data TestEnv = TestEnv { earnestConfig :: Config
                       }

instance IsOption TestEnv where
  defaultValue = error "MUST apply a config file"
  parseValue fp = Just $ TestEnv { earnestConfig = unsafePerformIO $ decodeFileThrow fp
                                 }
  optionName = return "config-file"
  optionHelp = return "The config file for running tests (in YAML format)"
  optionCLParser = mkOptionCLParser $ short 'c'

