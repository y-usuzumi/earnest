module Earnest.Exchange.TradeInfo where

import           Control.Monad.State
import           Data.Hashable
import qualified Data.HashMap.Lazy   as HM
import qualified Data.HashSet        as S
import           Data.List
import           Earnest.Currency
import           GHC.Generics

type TradeInfoLookup = HM.HashMap Currency (HM.HashMap Currency TradeInfo)

data TradeInfo = TradeInfo { fromCurrency :: Currency
                           , toCurrency   :: Currency
                           , fee          :: Double
                           } deriving (Eq, Generic, Hashable, Show)

newTradeInfoLookup :: TradeInfoLookup
newTradeInfoLookup = HM.empty

merge :: [(Currency, Currency, TradeInfo)] -> State TradeInfoLookup ()
merge pairs = do
  lookup <- get >>= return . flip (foldl' insertPair) pairs
  put lookup
  where
    insertPair :: TradeInfoLookup -> (Currency, Currency, TradeInfo) -> TradeInfoLookup
    insertPair lookup (cFrom, cTo, ti) =
      HM.insertWith mergeToCurrencyLookup cFrom (HM.singleton cTo ti) lookup
    mergeToCurrencyLookup = HM.unionWith (flip const)

fromList :: [(Currency, Currency, TradeInfo)] -> TradeInfoLookup
fromList pairs = execState (merge pairs) newTradeInfoLookup

isSupported :: (Currency, Currency) -> TradeInfoLookup -> Bool
isSupported (a, b) m
  | not (a `HM.member` m) = False
  | otherwise = let s = m HM.! a in b `HM.member` s
