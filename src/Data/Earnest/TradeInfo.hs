module Data.Earnest.TradeInfo where

import           Control.Monad.State
import           Data.Default
import           Data.Earnest.Currency
import           Data.Hashable
import qualified Data.HashMap.Lazy     as HM
import qualified Data.HashSet          as S
import           Data.List
import           GHC.Generics

type TradeInfoTable = HM.HashMap Currency (HM.HashMap Currency TradeInfo)

data TradeInfo = TradeInfo { fee   :: Double
                           , buy   :: Double
                           , sell  :: Double
                           , high  :: Double
                           , low   :: Double
                           , last  :: Double
                           , vol   :: Double
                           , depth :: Depth
                           }
               | RTradeInfo { fee   :: Double
                            , buy   :: Double
                            , sell  :: Double
                            , high  :: Double
                            , low   :: Double
                            , last  :: Double
                            , vol   :: Double
                            , depth :: Depth
                            }
               deriving (Generic, Show)

instance Default TradeInfo where
  def = TradeInfo { fee = 0
                  , buy = 0
                  , sell = 0
                  , high = 0
                  , low = 0
                  , last = 0
                  , vol = 0
                  , depth = Depth { asks = []
                                  , bids = []
                                  }
                  }

data Depth = Depth { asks :: [PO]
                   , bids :: [PO]
                   } deriving Show

instance Default Depth where
  def = Depth { asks = []
              , bids = []
              }

data PO = PO { price  :: Double
             , amount :: Double
             } deriving Show

newTradeInfoTable :: TradeInfoTable
newTradeInfoTable = HM.empty

merge :: [(Currency, Currency, TradeInfo)] -> State TradeInfoTable ()
merge pairs = do
  lookup <- gets $ flip (foldl' insertPair) pairs
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
