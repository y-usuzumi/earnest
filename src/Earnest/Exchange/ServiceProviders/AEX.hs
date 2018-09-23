module Earnest.Exchange.ServiceProviders.AEX where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.List
import           Earnest.Currency
import           Earnest.Exchange
import           Earnest.Exchange.CurrencyPair
import           Test.WebDriver
import           Test.WebDriver.Capabilities
import           Test.WebDriver.Commands.Wait

chromeConfig :: WDConfig
chromeConfig = foldl' (flip ($)) defaultConfig
  [ useBrowser chrome
  -- , useProxy $ Manual { httpProxy = "localhost:1080"
  --                     , ftpProxy = "localhost:1080"
  --                     , sslProxy = "localhost:1080"
  --                     }
  ]

data AEXExchange = AEXExchange

instance Exchange AEXExchange where
  loadInitialInfo _ = do
    liftIO $ void $ runSession chromeConfig . finallyClose $ do
      openPage "https://signin.gatehub.net/"
      username <- findElem $ ByName "gatehub_name"
      sendKeys "anohigisavay@gmail.com" username
      password <- findElem $ ByName "password"
      sendKeys "XXXXX" password
      form <- findElem $ ByTag "form"
      submit form
      control $ \runInIO -> do
        runInIO $ liftIO $ threadDelay 20000000
    return ExchangeInfo{ supportedCurrencyPairs = newExchangePairLookup
                       }
