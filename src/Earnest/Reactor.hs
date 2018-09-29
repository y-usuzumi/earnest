module Earnest.Reactor where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Foldable
import           Earnest.Action
import           Earnest.Exchange
import           Streamly               as S
import qualified Streamly.Prelude       as SP

data Reactor = Reactor { _exchanges :: [HExchange]
                       , _loop      :: Int
                       }

makeLenses ''Reactor

runReactor :: ( IsStream s
              , MonadTrans s
              , MonadIO (s m)
              , Monad mio
              , m ~ StateT Reactor mio
              ) => s (StateT Reactor mio) Ron
runReactor = do
  reactor <- lift $ get
  elems <- replicateM 5 $ do
    lift $ modify' (over loop (+1))
    return $ Ron [DummyAction $ length $ reactor ^. exchanges]
  SP.fromList elems
