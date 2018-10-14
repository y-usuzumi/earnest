module Test.Earnest where

import           Data.Aeson.TH
import           Data.IORef
import           Data.Yaml
import           GHC.Generics
import           System.IO.Unsafe

data BourseConfig = AEXBourseConfig { username :: String
                                    , password :: String
                                    }
                  | AEXAPIBourseConfig { uid  :: String
                                       , key  :: String
                                       , skey :: String
                                       }
                  | GateHubBourseConfig { username :: String
                                        , password :: String
                                        }
                  deriving (Generic, Show)

deriveJSON defaultOptions{ sumEncoding = defaultTaggedObject{ tagFieldName = "type"
                                                              }
                           , constructorTagModifier = \case
                               "AEXBourseConfig" -> "aex"
                               "AEXAPIBourseConfig" -> "aexapi"
                               "GateHubBourseConfig" -> "gatehub"
                               a -> a
                           } ''BourseConfig


data Config = Config { bourses :: [BourseConfig]
                     } deriving (Generic, Show, ToJSON)

config :: IORef Config
config = unsafePerformIO $ newIORef undefined
{-# NOINLINE config #-}
