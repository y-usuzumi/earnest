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

class (Ord e, Hashable e, Typeable e) => Exchange e where
  loadInfo :: MonadIO m => e -> m ExchangeInfo

data HExchange where
  HExchange :: Exchange e => e -> HExchange

instance Hashable HExchange where
  hashWithSalt a (HExchange e) = hashWithSalt a e

instance Eq HExchange where
  HExchange a == HExchange b = case cast b of
    Just b' -> a == b'
    Nothing -> False

instance Ord HExchange where
  compare (HExchange a) (HExchange b) = case cast b of
    Just b' -> compare a b'
    Nothing -> compare (typeOf a) (typeOf b)

data ExchangeInfo = ExchangeInfo { _supportedTrades :: TradeInfoLookup
                                 } deriving Show

makeLenses ''ExchangeInfo
