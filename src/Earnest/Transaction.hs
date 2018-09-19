module Earnest.Transaction where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch

newtype Transaction r = Transaction { action :: forall m. MonadThrow m => m r
                                    } deriving Functor

data TransactionException deriving Show

instance Exception TransactionException

-- instance Applicative Transaction where
--   pure = return
--   (<*>) = ap

-- instance Monad Transaction where
--   Transaction{..} >>= f = do
--     result <- async action
--     case result of
--       Left ex -> return $ Left ex
