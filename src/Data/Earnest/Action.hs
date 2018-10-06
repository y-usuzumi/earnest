module Data.Earnest.Action where

import           Control.Lens
import           Data.Earnest.Currency
import           Data.Earnest.Exchange
import           Streamly


data Action where
  DummyAction :: Int -> Action
  Trade :: Exchange e => e -> Currency -> Currency -> Action

data Ron where
  Ron :: { _actions :: (Monad m, IsStream s, Monad (s m)) => s m Action } -> Ron

makeLenses ''Action
makeLenses ''Ron
