module Earnest.Exchange.ServiceProviders.AEX
  ( AEXExchange(..)
  ) where

import           Control.Concurrent
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.List
import qualified Data.Text                     as T
import           Earnest.Currency
import           Earnest.Exchange
import           Earnest.Exchange.CurrencyPair
import           System.Environment
import           Test.WebDriver                hiding (browser)
import           Test.WebDriver.Commands.Wait

data Env = Env { username :: String
               , password :: String
               }

initEnv :: IO Env
initEnv = do
  username <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_AEX_USERNAME"
  password <- liftIO $ getEnv "EARNEST_SERVICEPROVIDER_AEX_PASSWORD"
  return Env{ username = username
            , password = password
            }

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

data AEXExchange = AEXExchange

instance Exchange AEXExchange where
  loadInitialInfo _ = do
    Env{..} <- liftIO initEnv
    liftIO $ void $ runSession wdConfig . finallyClose $ do
      openPage "https://www.aex.com/page/register.html?type=2"
      elemUsername <- findElem $ ById "my_self_email"
      sendKeys (T.pack username) elemUsername
      elemPassword <- findElem $ ByClass "password"
      sendKeys (T.pack password) elemPassword
      elemSubmit <- findElem $ ByClass "my_go_load"
      click elemSubmit
      control $ \runInIO -> do
        runInIO $ liftIO $ threadDelay 20000000
    return ExchangeInfo{ supportedCurrencyPairs = newExchangePairLookup
                       }
