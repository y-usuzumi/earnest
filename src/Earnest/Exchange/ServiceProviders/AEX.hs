module Earnest.Exchange.ServiceProviders.AEX
  ( AEXExchange(..)
  , AEXExchangeEnv(..)
  ) where

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Earnest.Currency
import           Earnest.Exchange
import           Earnest.Exchange.CurrencyPair
import           GHC.Generics
import           System.Environment
import           Test.WebDriver                hiding (browser)
import           Test.WebDriver.Commands.Wait

browser :: Browser
browser = chrome{ chromeOptions = ["--proxy-server=socks5://localhost:1080"]
                }

wdConfig :: WDConfig
wdConfig = foldl' (flip ($)) defaultConfig
  [ useBrowser browser
  ]

data AEXPage = AEXLogin
             | AEXPersonalCenter
             deriving (Eq, Generic, Show)

instance Hashable AEXPage

aexPageList :: HM.HashMap AEXPage String
aexPageList = HM.fromList
  [ (AEXLogin, "https://www.aex.com/page/register.html?type=2")
  , (AEXPersonalCenter, "https://www.aex.com/page/person_center.html")
  ]

data AEXExchange = AEXExchange

data AEXExchangeEnv = AEXExchangeEnv { username :: String
                                     , password :: String
                                     }

instance Exchange AEXExchange where
  type ExchangeEnv AEXExchange = AEXExchangeEnv

  loadInitialInfo _ AEXExchangeEnv{..} = do
    currencyPairs <- liftIO $ runSession wdConfig . finallyClose $ do
      openPage $ aexPageList HM.! AEXLogin
      elemUsername <- findElem $ ById "my_self_email"
      sendKeys (T.pack username) elemUsername
      elemPassword <- findElem $ ByClass "password"
      sendKeys (T.pack password) elemPassword
      elemSubmit <- findElem $ ByClass "my_go_load"
      click elemSubmit
      waitUntil 30 $ findElem $ ByClass "overview"
      openPage $ aexPageList HM.! AEXPersonalCenter
      currencyPairs <- getCurrencyPairs
      liftIO $ print currencyPairs
      return currencyPairs
      -- control $ \runInIO -> do
      --   runInIO $ liftIO $ threadDelay 20000000
    return ExchangeInfo{ supportedCurrencyPairs = fromList currencyPairs
                       }

    where
      getCurrencyPairs :: WD [(Currency, Currency)]
      getCurrencyPairs = do
        elemBalance <- findElem $ ById "myBalance"
        elemRegularAccountTabBtn <- findElemFrom elemBalance $ ByClass "nav-1"
        click elemRegularAccountTabBtn
        control $ \runInIO -> do
          runInIO $ liftIO $ threadDelay 1000000
        elemTable <- findElemFrom elemBalance (ByClass "table")
        elemRows <- findElemsFrom elemTable (ByTag "dd")
        fmap join $ forM elemRows $ \elemRow -> do
          elemPairs <- (findElemFrom elemRow (ByClass "markets_link") >>= \r -> do
                           findElemsFrom r (ByTag "li"))
            `catch` \(FailedCommand t _) ->
            case t of
              NoSuchElement -> return []
              _             -> liftIO $ print t >> return []
          cleanedPairs <- forM elemPairs (
            \elemPair -> fmap fromJust $ attr elemPair "innerHTML"
            ) >>= return . filter (not . T.null)
          forM cleanedPairs $ \elemPair -> do
            let pair = T.splitOn "/" elemPair
            case pair of
              (a:b:_) -> return (read $ T.unpack a, read $ T.unpack b)
              _       -> liftIO $ print pair >> error "WTF"
