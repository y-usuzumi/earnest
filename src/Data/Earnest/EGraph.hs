module Data.Earnest.EGraph where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Graph
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Maybe
import           Data.Earnest.Currency
import           Data.Earnest.Exchange
import           Data.Earnest.Exchange.TradeInfo
import           GHC.Generics


data Node = NCurrency Currency
          | NExchange HExchange Currency Currency TradeInfo
          deriving (Eq, Generic)

instance Hashable Node

data Key = KCurrency Currency
         | KExchange HExchange Currency Currency
         deriving (Eq, Ord)

type EGraph = (Graph, Vertex -> (Node, Key, [Key]), Key -> Maybe Vertex)

graphFromExchanges :: [(HExchange, ExchangeInfo)] -> EGraph
graphFromExchanges = graphFromEdges . toEdges
  where
    toEdges :: [(HExchange, ExchangeInfo)] -> [(Node, Key, [Key])]
    toEdges = toNKKs . foldl' folder HM.empty

    folder hm (x, xi) =
      let tradeInfos = toList (xi ^. supportedTrades)
      in foldl' (tiFolder x) hm tradeInfos

    tiFolder x hm (i, o, ti) = flip execState hm $ do
      modify (HM.insertWith qmerge (NCurrency i) [NExchange x i o ti])
      modify (HM.insertWith qmerge (NExchange x i o ti) [NCurrency o])

    qmerge a b = reverse $ a ++ b

    toNKKs = HM.foldrWithKey nkkFolder []

    nkkFolder k a l = flip (:) l $ (k, n2k k, map n2k a)

    n2k (NCurrency curr)    = KCurrency curr
    n2k (NExchange x i o _) = KExchange x i o

isCurrencySupported :: Currency -> EGraph -> Bool
isCurrencySupported c (_, _, k2v) = isJust $ k2v (KCurrency c)

getTradableOptions :: Currency -> EGraph -> [(HExchange, Currency, Currency, TradeInfo)]
getTradableOptions c (_, v2n, k2v) = case k2v (KCurrency c) of
  Just v -> map (n2to . view _1 . v2n . fromJust . k2v) (view _3 $ v2n v)
  Nothing -> []
  where
    n2to (NCurrency _) = error "Malformed graph: currency nodes connect to each other"
    n2to (NExchange x i o ti) = (x, i, o, ti)
