module Earnest.Exchange.ServiceProviders.GateHub where

import           Control.Monad.State
import           Earnest.Currency
import           Earnest.Exchange
import           Earnest.Exchange.CurrencyPair

data GateHubExchange = GateHubExchange

instance Exchange GateHubExchange where
  loadInitialInfo _ = do
    let currencyPairs = flip execState newExchangePairLookup $ do
          merge [ (CNY, XRP)
                , (XRP, CNY)
                ]
    return ExchangeInfo{ supportedCurrencyPairs = currencyPairs
                       }
