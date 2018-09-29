module Earnest.Exchange where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.HashMap.Strict        as HM
import qualified Data.Set                   as S
import           Earnest.Currency
import           Earnest.Exchange.TradeInfo
import           Earnest.Transaction


class Exchange e where
  type ExchangeEnv e

  loadInitialInfo :: MonadIO m => e -> ExchangeEnv e -> m ExchangeInfo

data ExchangeInfo = ExchangeInfo { _supportedTrades :: TradeInfoLookup
                                 } deriving Show

makeLenses ''ExchangeInfo
