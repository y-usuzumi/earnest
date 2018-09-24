module Earnest.Exchange.CurrencyPair where

import           Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as S
import           Data.List
import           Earnest.Currency

type ExchangePairLookup = HM.HashMap Currency (S.HashSet Currency)

newExchangePairLookup :: ExchangePairLookup
newExchangePairLookup = HM.empty

merge :: [(Currency, Currency)] -> State ExchangePairLookup ()
merge pairs = do
  lookup <- get >>= return . flip (foldl' insertPair) pairs
  put lookup
  where
    insertPair :: ExchangePairLookup -> (Currency, Currency) -> ExchangePairLookup
    insertPair lookup pair =
      HM.insertWith S.union (fst pair) (S.singleton $ snd pair) lookup

fromList :: [(Currency, Currency)] -> ExchangePairLookup
fromList pairs = execState (merge pairs) newExchangePairLookup

isSupported :: (Currency, Currency) -> ExchangePairLookup -> Bool
isSupported (a, b) m
  | not (a `HM.member` m) = False
  | otherwise = let s = m HM.! a in b `S.member` s
