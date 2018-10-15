module Test.Earnest.Env where

import Earnest.Config

data TestEnv = TestEnv { earnestConfig :: Config
                       }
