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
import           Prelude                          hiding (last)

type Q p = forall m. MonadIO m => B.Pipe -> p -> m [B.Record]
type Q_ p = forall m. MonadIO m => B.Pipe -> p -> m ()

qMerge :: Q_ (UUID, (Currency, HBourse), (Currency, HBourse), TradeInfo)
qMerge p (gid, (fc, fb), (tc, tb), ti) = do
  liftIO $ putStrLn [i|Merging #{fc} -> #{tc}|]
  mergeNode p gid fc fb
  mergeNode p gid tc tb
  mergeRelation p gid fc fb tc tb
  where
    mergeNodeStmt = [i|
                      MERGE (:Currency {
                      gid: $gid,
                      c: $c,
                      b: $b
                      })
                      |]
    mergeRelationStmt = [i|
                          MATCH (f:Currency {
                          gid: $gid,
                          c: $fc,
                          b: $fb
                          }), (t:Currency {
                          gid: $gid,
                          c: $tc,
                          b: $tb
                          })
                          MERGE (f)-[:TradeInfo{
                            f:$fee,
                            b:$buy,
                            s:$sell,
                            h:$high,
                            l:$low,
                            p:$last,
                            v:$vol,
                            d:[]
                            }]->(t)
                          |]
    mergeNode p gid c b =
      B.run p $ B.queryP_ mergeNodeStmt $ M.fromList [ ("gid", B.T $ T.pack $ show gid)
                                                     , ("c", B.T $ T.pack $ show c)
                                                     , ("b", B.T $ T.pack $ show b)
                                                     ]
    mergeRelation p gid fc fb tc tb =
      B.run p $ B.queryP_ mergeRelationStmt $ M.fromList [ ("gid", B.T $ T.pack $ show gid)
                                                         , ("fc", B.T $ T.pack $ show fc)
                                                         , ("fb", B.T $ T.pack $ show fb)
                                                         , ("tc", B.T $ T.pack $ show tc)
                                                         , ("tb", B.T $ T.pack $ show tb)
                                                         , ("fee", B.F $ fee ti)
                                                         , ("buy", B.F $ buy ti)
                                                         , ("sell", B.F $ sell ti)
                                                         , ("high", B.F $ high ti)
                                                         , ("low", B.F $ low ti)
                                                         , ("last", B.F $ last ti)
                                                         , ("vol", B.F $ vol ti)
                                                         ]

newtype Neo4jGraph = Neo4jGraph UUID
                   deriving Show

instance EGraph Neo4jGraph where
  type Driver Neo4jGraph = B.Pipe

  graphFromBourses driver bourses = do
    gid <- liftIO nextRandom
    bis <- mapM (\(HBourse b) -> loadInfo b) bourses
    forM_ (zip bourses bis) $ \(b, bi) -> do
      let ls = TI.toList $ bi ^. supportedTrades
      forM_ ls $ \(f, t, ti) -> do
        qMerge driver (gid, (f, b), (t, b), ti)
    return $ Neo4jGraph gid

  getTradableOptions = undefined

  findProfitablePaths = undefined

  explain _ = print
