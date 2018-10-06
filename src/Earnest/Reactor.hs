module Earnest.Reactor where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Foldable
import           Data.Earnest.Action
import           Data.Earnest.Exchange
import           Streamly               as S
import qualified Streamly.Prelude       as S

data Reactor = Reactor { _exchanges :: [HExchange]
                       , _loop      :: Int
                       }

makeLenses ''Reactor

runReactor :: ( IsStream s
              , MonadTrans s
              , MonadIO (s m)
              , Monad mio
              , m ~ StateT Reactor mio
              , IsStream s'
              , Monad (s' Identity)
              ) => s (StateT Reactor mio) (Ron s' Identity)
runReactor = do
  reactor <- lift $ get
  elems <- replicateM 5 $ do
    lift $ modify' (over loop (+1))
    let xCnt = length $ reactor ^. exchanges
    return $ Ron $ serially $ (S.fromList [DummyAction xCnt] :: SerialT Identity Action)
  serially $ S.fromList elems
