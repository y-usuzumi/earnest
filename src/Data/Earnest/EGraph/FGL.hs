module Data.Earnest.EGraph.FGL where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Earnest.Bourse
import           Data.Earnest.Currency
import           Data.Earnest.EGraph
import           Data.Earnest.ExchangeInfo
import           Data.Earnest.TradeInfo
import           Data.Earnest.TransferInfo
import           Data.Earnest.Wallet       hiding (Currency)
import           Data.Graph.Inductive
import           Data.List


type FGLGraph = Gr ENodeInfo EEdgeInfo

instance EGraph FGLGraph where
  -- FIXME: Nodes should be uniquely identified by (Currency, HBourse) pairs
  graphFromBourses bourses = do
    bis <- mapM (\(HBourse b) -> loadInfo b) bourses
    let b_bi_pairs = zip bourses bis
        b_c_c_ti_pairs = map (fst &&& toList . view supportedTrades . snd) b_bi_pairs
    return $ mkGraph (join $ map mkNodes b_c_c_ti_pairs) (join $ map mkEdges b_c_c_ti_pairs)
    where
      mkNodes (x, cctis) = map ( fromEnum
                                 &&&
                                 \curr -> ENodeInfo{ _currency = curr
                                                   , _exchange = x
                                                   }
                               ) $ foldl' folder [] cctis
      mkEdges (x, cctis) = map (curry mkEdge x) cctis
      mkEdge (x, (f, t, ti)) = (fromEnum f, fromEnum t, ETrade x ti)
      folder l (f, t, ti) = f:t:l

  getTradableOptions c g = map (r c) $ lsuc g (fromEnum c)
    where
      r c (n, ETrade x ti) = (x, c, toEnum n, ti)

  findProfitablePaths = undefined

isCurrencySupported :: Currency -> FGLGraph -> Bool
isCurrencySupported c = gelem (fromEnum c)

explain :: FGLGraph -> IO ()
explain = prettyPrint
