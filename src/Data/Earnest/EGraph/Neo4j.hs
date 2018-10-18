module Data.Earnest.EGraph.Neo4j where


import           Data.Earnest.EGraph
import qualified Data.Map.Lazy       as M
import           Data.UUID.V4
import qualified Database.Bolt       as B

type Q = MonadIO m => B.Pipe -> Map Text Value -> m [Record]
type Q_ = MonadIO m => B.Pipe -> Map Text Value -> m ()

qMerge :: MonadIO m => Pipe -> Currency -> m ()
qMerge p curr = B.runP p $ queryP_ "MERGE (p:$curr)" $ M.fromList [ ("curr", show curr)
                                                                    ]

data Neo4jGraph = Neo4jGraph { host     :: String
                             , port     :: Int
                             , username :: String
                             , password :: String
                             }

getBoltCfg :: Neo4jGraph -> B.BoltCfg
getBoltCfg Neo4jGraph{..} = B.def { B.host = host
                                  , B.port = port
                                  , B.user = username
                                  , B.password = password
                                  }

instance EGraph Neo4jGraph where
  graphFromBourses bourses = do
    uuid <- nextRandom
    p <- B.connect $ getBoltCfg
    bis <- mapM (\(HBourse b) -> loadInfo b) bourses
    forM_ bis $ \bi -> do
      let ls = toList $ bi ^. supportedTrades
      forM_ ls $ \(f, t, _) -> do
        qCreate p (show f)
        qCreate p (show t)
