module Data.Earnest.Bourse where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Earnest.Balance
import           Data.Earnest.Currency
import           Data.Earnest.TradeInfo
import           Data.Earnest.Transaction
import           Data.Hashable
import qualified Data.HashMap.Strict      as HM
import qualified Data.Set                 as S
import           Data.Typeable
import           Text.Printf

class (Hashable e, Ord e, Show e, Typeable e) => Bourse e where
  loadInfo :: MonadIO m => e -> m BourseInfo

data HBourse where
  HBourse :: Bourse e => e -> HBourse

instance Show HBourse where
  showsPrec d (HBourse e) = showParen (d > 10) $ showString "HBourse " . showsPrec 11 e

instance Hashable HBourse where
  hashWithSalt a (HBourse e) = hashWithSalt a e

instance Eq HBourse where
  HBourse a == HBourse b = case cast b of
    Just b' -> a == b'
    Nothing -> False

instance Ord HBourse where
  compare (HBourse a) (HBourse b) = case cast b of
    Just b' -> compare a b'
    Nothing -> compare (typeOf a) (typeOf b)

data BourseInfo = BourseInfo { _supportedTrades :: TradeInfoTable
                             , _balances        :: BalanceTable
                             , _confidence      :: Double
                             } deriving Show

makeLenses ''BourseInfo
