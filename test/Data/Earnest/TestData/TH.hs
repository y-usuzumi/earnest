module Data.Earnest.TestData.TH where

import Data.Earnest.Exchange
import Language.Haskell.TH


hxsFromNames :: [ExpQ] -> ExpQ
hxsFromNames s = listE $ map (\exp -> [|HExchange $(exp)|]) s

xxisFromNames :: [ExpQ] -> ExpQ
xxisFromNames s = [|mapM (\h@(HExchange x) -> loadInfo x >>= \y -> return (h, y)) $(hxsFromNames s)|]
