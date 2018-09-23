module Earnest.Exchange where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.HashMap.Strict           as HM
import qualified Data.Set                      as S
import           Earnest.Currency
import           Earnest.Exchange.CurrencyPair
import           Earnest.Order
import           Earnest.Protocol
import           Earnest.Transaction


class Exchange e where
  loadInitialInfo :: MonadIO m => e -> m ExchangeInfo

data ExchangeInfo = ExchangeInfo { supportedCurrencyPairs :: ExchangePairLookup
                                 }
