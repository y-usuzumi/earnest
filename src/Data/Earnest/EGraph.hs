module Data.Earnest.EGraph where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Earnest.Bourse
import           Data.Earnest.Currency
import           Data.Earnest.Wallet       hiding (Currency)
import           Data.Earnest.ExchangeInfo
import           Data.Earnest.TradeInfo
import           Data.Earnest.TransferInfo



data ENodeInfo = ENodeInfo { _currency :: Currency
                           , _exchange :: HBourse
                           } deriving Show

makeLenses ''ENodeInfo

data EEdgeInfo = ETrade HBourse TradeInfo
               | ETransfer HWallet TransferInfo  -- TODO: Verify this
               | EExchange HBourse ExchangeInfo
               deriving Show


class EGraph g where
  graphFromBourses :: MonadIO m => [HBourse] -> m g
  getTradableOptions :: Currency -> g -> [(HBourse, Currency, Currency, TradeInfo)]
  findProfitablePaths :: g -> [(ENodeInfo, EEdgeInfo, ENodeInfo)]