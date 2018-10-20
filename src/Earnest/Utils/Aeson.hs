module Earnest.Utils.Aeson where

import           Control.Monad.Catch
import           Data.Aeson
import           Data.ByteString.Lazy
import           Earnest.Utils

newtype JSONParseException = JSONParseException String
                           deriving Show

instance Exception JSONParseException

decodeThrow :: (ThrowableIO m, FromJSON a) => ByteString -> m a
decodeThrow b = case eitherDecode b of
  Left msg -> throwM $ JSONParseException msg
  Right a  -> return a
