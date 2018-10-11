module Earnest.Exchange.ServiceProviders.AEX
  ( AEXExchange(..)
  ) where

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Earnest.Currency
import           Data.Earnest.Exchange
import           Data.Earnest.Exchange.Balance
import           Data.Earnest.Exchange.TradeInfo
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HM
import           Data.List
import           Data.Maybe
import           Data.Maybe
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Earnest.WebDriver.Utils
import           GHC.Generics
import           System.Environment
import           Test.WebDriver                  hiding (browser)
import           Test.WebDriver.Commands.Wait
import           Text.Printf

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

data AEXExchange = AEXExchange { username :: String
                               , password :: String
                               } deriving (Eq, Generic, Ord, Show)

instance Hashable AEXExchange

instance Exchange AEXExchange where
  confidence _ = return 0.2
  loadInfo AEXExchange{..} = do
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
      liftIO $ putStrLn "Getting currency info"
      currencyPairs <- getCurrencyPairs
      liftIO $ print currencyPairs
      return currencyPairs
      -- control $ \runInIO -> do
      --   runInIO $ liftIO $ threadDelay 20000000
    return ExchangeInfo{ _supportedTrades = newTradeInfoTable
                       , _balances = newBalanceTable  -- FIXME
                       }

    where
      getCurrencyPairs :: WD [(Currency, Currency)]
      getCurrencyPairs = do
        liftIO $ putStrLn "Finding myBalance page"
        elemBalance <- findElem $ ById "myBalance"
        liftIO $ putStrLn "Finding nav-1 tab"
        elemRegularAccountTabBtn <- findElemFrom elemBalance $ ByClass "nav-1"
        liftIO $ putStrLn "Clicking on this tab"
        click elemRegularAccountTabBtn
        retry 5 $ do
          liftIO $ putStrLn "Finding the table <dl>"
          elemTable <- waitUntil 30 $ findElemFrom elemBalance (ByTag "dl")
          liftIO $ putStrLn "Finding rows <dd>"
          liftIO $ threadDelay 1000000
          elemRows <- waitUntil 30 $ findElemsFrom elemTable (ByTag "dd")
          fmap join $ forM elemRows $ \elemRow -> (`runContT` id) $ do
            response <- callCC $ \exit -> do
              lift $ liftIO $ putStrLn "Finding markets_link"
              maybeElemMarketLink <- lift $ maybeNotFound $ findElemFrom elemRow (ByClass "markets_link")
              when (isNothing maybeElemMarketLink) $ exit []
              let elemMarketLink = fromJust maybeElemMarketLink
              liftIO $ putStrLn "Finding <li>'s'"
              maybeElemPairs <- lift $ maybeNotFound $ findElemsFrom elemMarketLink (ByTag "li")
              when (isNothing maybeElemPairs) $ exit []
              let elemPairs = fromJust maybeElemPairs
              lift $ do
                cleanedPairs <- forM elemPairs (
                  \elemPair -> fmap fromJust $ attr elemPair "innerText"
                  ) >>= return . filter (not . T.null)
                maybeValidPairs <- forM cleanedPairs $ \elemPair -> do
                  let pair = T.splitOn "/" elemPair
                  case pair of
                    (a:b:_) -> return $ Just (read $ T.unpack a, read $ T.unpack b)
                    _       -> liftIO $ printf "Invalid currency pair input: %s" elemPair >> return Nothing
                return $ catMaybes maybeValidPairs
            return $ return response
