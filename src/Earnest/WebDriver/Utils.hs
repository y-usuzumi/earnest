module Earnest.WebDriver.Utils where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Test.WebDriver
import           Text.Printf

retry :: (MonadIO m, MonadCatch m) => Int -> m a -> m a
retry 0 action = liftIO (putStrLn "Retrying") >> action
retry n action
  | n < 0 = error "retry times < 0"
  | otherwise = liftIO (printf "Retrying %d\n" n) >> action `catchAll` (const $ retry (n-1) action)

maybeNotFound :: WD a -> WD (Maybe a)
maybeNotFound action = (Just <$> action)
  `catch` \t -> case t of
                  (FailedCommand NoSuchElement _) -> liftIO (putStrLn "Not found") >> return Nothing
                  e -> throwM e
