module Earnest.Utils where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

type ThrowableIO m = (MonadThrow m, MonadIO m)
