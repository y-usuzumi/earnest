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

data BourseException = LoadInfoFailed String
                     deriving Show

instance Exception BourseException

class (Hashable e, Show e, Typeable e) => Bourse e where
  loadInfo :: MonadIO m => e -> m BourseInfo

data HBourse where
  HBourse :: Bourse e => e -> HBourse

instance Show HBourse where
  showsPrec d (HBourse e) = showParen (d > 10) $ showString "HBourse " . showsPrec 11 e

instance Hashable HBourse where
  hashWithSalt a (HBourse e) = hashWithSalt a e

data BourseInfo = BourseInfo { _supportedTrades :: TradeInfoTable
                             , _balances        :: BalanceTable
                             , _confidence      :: Double
                             } deriving Show

makeLenses ''BourseInfo
