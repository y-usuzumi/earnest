module Data.Earnest.Action where

import           Control.Lens
import           Data.Earnest.Currency
import           Data.Earnest.Bourse
import           Streamly


data Action where
  DummyAction :: Int -> Action
  Trade :: HBourse -> Currency -> Currency -> Action

data Ron s m where
  Ron :: (IsStream s, Monad (s m)) => s m Action -> Ron s m

-- NOTE: There would be NO actions lens for these types because the 's'
-- and 'm' type constructors would escape from the scope
