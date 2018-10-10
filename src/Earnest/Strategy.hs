module Earnest.Strategy where

import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Data.Earnest.Action
import           Data.Earnest.EGraph.FGL
import           Streamly               as S
import qualified Streamly.Prelude       as S

findRons :: (IsStream s, Monad (s Identity)) => EGraph -> s Identity (Ron s Identity)
findRons g = do
  return $ Ron $ serially (S.fromList [] :: SerialT Identity a)
