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

qMerge :: Q_ [(UUID, (Currency, HBourse), (Currency, HBourse), TradeInfo)]
qMerge p ls = liftIO $ do
  let params = flip map ls $ \(gid, (fc, fb), (tc, tb), ti) ->
        B.M $ M.fromList [ ("gid", B.T $ T.pack $ show gid)
                         , ("fc", B.T $ T.pack $ show fc)
                         , ("fb", B.T $ T.pack $ show fb)
                         , ("tc", B.T $ T.pack $ show tc)
                         , ("tb", B.T $ T.pack $ show tb)
                         , ("fee", B.F $ fee ti)
                         , ("buy", B.F $ buy ti)
                         , ("sell", B.F $ sell ti)
                         , ("high", B.F $ high ti)
                         , ("low", B.F $ fee ti)
                         , ("last", B.F $ last ti)
                         , ("vol", B.F $ vol ti)
                         ]
  B.run p $ B.queryP_ stmt $ M.singleton "l" (B.L params)
  where
    stmt = [i|
             UNWIND $l AS pair
             MERGE (m:Currency {
             gid: pair.gid,
             c: pair.fc,
             b: pair.fb
             })
             MERGE (n:Currency {
             gid: pair.gid,
             c: pair.tc,
             b: pair.tb
             })
             MERGE (m)-[:TradeInfo{
               f:pair.fee,
               b:pair.buy,
               s:pair.sell,
               h:pair.high,
               l:pair.low,
               p:pair.last,
               v:pair.vol,
               d:[]
               }]->(n)
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
      qMerge driver $ map (\(f, t, ti) -> (gid, (f, b), (t, b), ti)) ls
    return $ Neo4jGraph gid

  getTradableOptions = undefined

  findProfitablePaths = undefined

  explain _ = print
