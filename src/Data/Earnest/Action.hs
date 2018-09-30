module Data.Earnest.Action where

import           Control.Lens
import           Data.Earnest.Currency
import           Data.Earnest.Exchange


data Action where
  DummyAction :: Int -> Action
  Trade :: Exchange e => e -> Currency -> Currency -> Action

data Ron = Ron { _actions :: [Action]
               }

makeLenses ''Action
makeLenses ''Ron
