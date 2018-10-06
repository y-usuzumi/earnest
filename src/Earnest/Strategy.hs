module Earnest.Strategy where

import           Data.Earnest.Action
import           Data.Earnest.EGraph
import           Streamly            as S
import qualified Streamly.Prelude    as S

findRons :: (Monad m, IsStream s, Monad (s m)) => EGraph -> s m Ron
findRons g = do
  return Ron{ _actions = S.fromList []
            }
