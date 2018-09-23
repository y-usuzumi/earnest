module Earnest.Exchange where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.HashMap.Strict    as HM
import qualified Data.Set               as S
import           Earnest.Currency
import           Earnest.Order
import           Earnest.Protocol
import           Earnest.Transaction


class Exchange e where
  type Protocol e :: *

data ExchangeState = ExchangeState { supportedExchanges :: String
                                   }
