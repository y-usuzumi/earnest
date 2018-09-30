module Earnest.Exchange.ServiceProviders.GateHub
  ( GateHubExchange(..)
  ) where

import           Control.Concurrent
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Hashable
import           Data.List
import qualified Data.Text                    as T
import           Data.Earnest.Currency
import           Data.Earnest.Exchange
import           Data.Earnest.Exchange.TradeInfo
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

data GateHubExchange = GateHubExchange { username :: String
                                       , password :: String
                                       } deriving (Eq, Generic, Ord, Show)

instance Hashable GateHubExchange

instance Exchange GateHubExchange where
  loadInfo GateHubExchange{..} = do
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
    return ExchangeInfo{ _supportedTrades = newTradeInfoLookup
                       }
