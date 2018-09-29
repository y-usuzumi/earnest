module Earnest.Exchange where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import qualified Data.Set                   as S
import           Data.Typeable
import           Earnest.Currency
import           Earnest.Exchange.TradeInfo
import           Earnest.Transaction

class (Eq e, Hashable e, Typeable e) => Exchange e where
  loadInfo :: MonadIO m => e -> m ExchangeInfo

data HExchange where
  HExchange :: Exchange e => e -> HExchange

instance Eq HExchange where
  HExchange a == HExchange b = case cast b of
    Just b' -> a == b'
    Nothing -> False

data ExchangeInfo = ExchangeInfo { _supportedTrades :: TradeInfoLookup
                                 } deriving Show

makeLenses ''ExchangeInfo
