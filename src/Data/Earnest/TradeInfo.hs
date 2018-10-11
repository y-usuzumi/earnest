module Data.Earnest.TradeInfo where

import           Control.Monad.State
import           Data.Hashable
import qualified Data.HashMap.Lazy   as HM
import qualified Data.HashSet        as S
import           Data.List
import           Data.Earnest.Currency
import           GHC.Generics

type TradeInfoTable = HM.HashMap Currency (HM.HashMap Currency TradeInfo)

data TradeInfo = TradeInfo { fee          :: Double
                           } deriving (Eq, Generic, Hashable, Show)

newTradeInfoTable :: TradeInfoTable
newTradeInfoTable = HM.empty

merge :: [(Currency, Currency, TradeInfo)] -> State TradeInfoTable ()
merge pairs = do
  lookup <- get >>= return . flip (foldl' insertPair) pairs
  put lookup
  where
    insertPair :: TradeInfoTable -> (Currency, Currency, TradeInfo) -> TradeInfoTable
    insertPair lookup (cFrom, cTo, ti) =
      HM.insertWith mergeToCurrencyLookup cFrom (HM.singleton cTo ti) lookup
    mergeToCurrencyLookup = HM.unionWith (flip const)

fromList :: [(Currency, Currency, TradeInfo)] -> TradeInfoTable
fromList pairs = execState (merge pairs) newTradeInfoTable

isSupported :: (Currency, Currency) -> TradeInfoTable -> Bool
isSupported (a, b) m
  | not (a `HM.member` m) = False
  | otherwise = let s = m HM.! a in b `HM.member` s

toList :: TradeInfoTable -> [(Currency, Currency, TradeInfo)]
toList til = join $ map (\(k1, m) -> map (\(k2, ti) -> (k1, k2, ti)) $ HM.toList m) $
             HM.toList til
