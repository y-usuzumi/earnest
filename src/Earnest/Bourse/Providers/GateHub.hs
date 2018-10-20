module Earnest.Bourse.Providers.GateHub
  ( GateHubBourse(..)
  ) where

import           Control.Concurrent
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Earnest.Balance
import           Data.Earnest.Bourse
import           Data.Earnest.Currency
import           Data.Earnest.TradeInfo
import           Data.Hashable
import           Data.List
import qualified Data.Text                    as T
import           GHC.Generics
import           System.Environment
import           Test.WebDriver               hiding (browser)
import           Test.WebDriver.Commands.Wait

browser :: Browser
browser = chrome{ chromeOptions = ["--proxy-server=socks5://localhost:1080"]
                }

wdConfig :: WDConfig
wdConfig = foldl' (flip ($)) defaultConfig
  [ useBrowser browser
  -- , useProxy $ Manual { httpProxy = "localhost:1080"
  --                     , ftpProxy = "localhost:1080"
  --                     , sslProxy = "localhost:1080"
  --                     }
  ]

data GateHubBourse = GateHubBourse { username :: String
                                   , password :: String
                                   } deriving (Eq, Generic, Ord, Show)

instance Hashable GateHubBourse

instance Bourse GateHubBourse where
  loadInfo GateHubBourse{..} = do
    liftIO $ void $ runSession wdConfig . finallyClose $ do
      openPage "https://signin.gatehub.net/"
      elemUsername <- findElem $ ByName "gatehub_name"
      sendKeys (T.pack username) elemUsername
      elemPassword <- findElem $ ByName "password"
      sendKeys (T.pack password) elemPassword
      elemForm <- findElem $ ByTag "form"
      submit elemForm
      control $ \runInIO -> do
        runInIO $ liftIO $ threadDelay 20000000
    return BourseInfo{ _supportedTrades = newTradeInfoTable
                     , _balances = newBalanceTable  -- FIXME
                     , _confidence = 1.0
                     }
  updateBourseInfo = undefined
