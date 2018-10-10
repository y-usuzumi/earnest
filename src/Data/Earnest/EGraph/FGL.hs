module Data.Earnest.EGraph.FGL where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Earnest.Currency
import           Data.Earnest.Exchange
import           Data.Earnest.Exchange.TradeInfo
import           Data.Graph.Inductive
import           Data.List

data ENode = ECurrency Currency deriving Show
data EEdge = EExchange HExchange TradeInfo deriving Show
type EGraph = Gr ENode EEdge

graphFromExchanges :: [(HExchange, ExchangeInfo)] -> EGraph
graphFromExchanges xxiPairs = let
  xCCTIPairs = map (fst &&& toList . view supportedTrades . snd) xxiPairs
  in mkGraph (join $ map (mkNodes . snd) xCCTIPairs) (join $ map mkEdges xCCTIPairs)
  where
    mkNodes cctis = map (fromEnum &&& ECurrency) $ foldl' folder [] cctis
    mkEdges (x, cctis) = map mkEdge (zip (repeat x) cctis)
    mkEdge (x, (f, t, ti)) = (fromEnum f, fromEnum t, EExchange x ti)
    folder l (f, t, ti) = f:t:l

isCurrencySupported :: Currency -> EGraph -> Bool
isCurrencySupported c g = gelem (fromEnum c) g

getTradableOptions :: Currency -> EGraph -> [(HExchange, Currency, Currency, TradeInfo)]
getTradableOptions c g = map (r c) $ lsuc g (fromEnum c)
  where
    r c (n, (EExchange x ti)) = (x, c, toEnum n, ti)

explain :: EGraph -> IO ()
explain = prettyPrint
