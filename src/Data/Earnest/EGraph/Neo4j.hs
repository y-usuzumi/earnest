module Data.Earnest.EGraph.Neo4j where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Earnest.Bourse
import           Data.Earnest.Currency
import           Data.Earnest.EGraph
import           Data.Earnest.TradeInfo           as TI
import qualified Data.Map.Lazy                    as M
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as T
import           Data.UUID
import           Data.UUID.V4
import qualified Database.Bolt                    as B

type Q p = forall m. MonadIO m => B.Pipe -> p -> m [B.Record]
type Q_ p = forall m. MonadIO m => B.Pipe -> p -> m ()

qMerge :: Q_ (Currency, HBourse)
qMerge p (currency, bourse) = let
  in B.run p $ B.queryP_ stmt $ M.fromList [ ("c", B.T $ T.pack $ show currency)
                                           , ("b", B.T $ T.pack $ show bourse)
                                           ]
  where
    stmt = [i|
             MERGE (p:Currency {
               c: $c,
               b: $b
             })
             |]

newtype Neo4jGraph = Neo4jGraph UUID
                   deriving Show

instance EGraph Neo4jGraph where
  type Driver Neo4jGraph = B.Pipe

  graphFromBourses driver bourses = do
    uuid <- liftIO nextRandom
    bis <- mapM (\(HBourse b) -> loadInfo b) bourses
    forM_ (zip bourses bis) $ \(b, bi) -> do
      let ls = TI.toList $ bi ^. supportedTrades
      forM_ ls $ \(f, t, _) -> do
        qMerge driver (f, b)
        qMerge driver (t, b)

    return $ Neo4jGraph uuid

  getTradableOptions = undefined
  findProfitablePaths = undefined
  explain _ = print
