module Earnest.Reactor where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Foldable
import           Earnest.Exchange
import           Streamly               as S
import qualified Streamly.Prelude       as SP

data HExchange where
  HExchange :: Exchange e => e -> HExchange

data Action = Action { _i :: Int
                     }

data Ron = Ron { _actions :: [Action]
               }

data Reactor = Reactor { _exchanges :: [HExchange]
                       , _loop      :: Int
                       }

makeLenses ''Action
makeLenses ''Ron
makeLenses ''Reactor

runReactor :: ( IsStream s
              , MonadTrans s
              , MonadIO mio
              , MonadIO (s m)
              , m ~ StateT Reactor mio
              ) => s (StateT Reactor mio) Ron
runReactor = do
  reactor <- lift $ get
  elems <- replicateM 5 $ do
    liftIO $ threadDelay 1000000
    lift $ modify' (over loop (+1))
    return $ Ron [Action $ length $ reactor ^. exchanges]
  SP.fromList elems
