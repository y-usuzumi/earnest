module Data.Earnest.Exchange where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Earnest.Currency
import           Data.Earnest.Exchange.TradeInfo
import           Data.Earnest.Transaction
import           Data.Hashable
import qualified Data.HashMap.Strict             as HM
import qualified Data.Set                        as S
import           Data.Typeable
import           Text.Printf

class (Hashable e, Ord e, Show e, Typeable e) => Exchange e where
  confidence :: MonadIO m => e -> m Double
  confidence _ = return 1
  loadInfo :: MonadIO m => e -> m ExchangeInfo

data HExchange where
  HExchange :: Exchange e => e -> HExchange

instance Show HExchange where
  showsPrec d (HExchange e) = showParen (d > 10) $ showString "HExchange " . showsPrec 11 e

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
