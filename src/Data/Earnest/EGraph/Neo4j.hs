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

qMerge :: Q_ (UUID, (Currency, HBourse), (Currency, HBourse))
qMerge p (gid, (fc, fb), (tc, tb)) = do
  B.run p $ B.queryP_ stmt $ M.fromList [ ("gid", B.T $ T.pack $ show gid)
                                        , ("fc", B.T $ T.pack $ show fc)
                                        , ("fb", B.T $ T.pack $ show fb)
                                        , ("tc", B.T $ T.pack $ show tc)
                                        , ("tb", B.T $ T.pack $ show tb)
                                        ]
  where
    stmt = [i|
             MERGE (p:Currency {
               gid: $gid,
               c: $fc,
               b: $fb
             })-[:t]->(q:Currency {
               gid: $gid,
               c: $tc,
               b: $tb
             })
             |]

newtype Neo4jGraph = Neo4jGraph UUID
                   deriving Show

instance EGraph Neo4jGraph where
  type Driver Neo4jGraph = B.Pipe

  graphFromBourses driver bourses = do
    gid <- liftIO nextRandom
    bis <- mapM (\(HBourse b) -> loadInfo b) bourses
    forM_ (zip bourses bis) $ \(b, bi) -> do
      let ls = TI.toList $ bi ^. supportedTrades
      forM_ ls $ \(f, t, _) -> do
        qMerge driver (gid, (f, b), (t, b))
    return $ Neo4jGraph gid

  getTradableOptions = undefined

  findProfitablePaths = undefined

  explain _ = print
