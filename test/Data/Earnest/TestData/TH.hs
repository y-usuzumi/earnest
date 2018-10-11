module Data.Earnest.TestData.TH where

import           Data.Earnest.Bourse
import           Language.Haskell.TH


hxsFromNames :: [ExpQ] -> ExpQ
hxsFromNames s = listE $ map (\exp -> [|HBourse $(exp)|]) s

xxisFromNames :: [ExpQ] -> ExpQ
xxisFromNames s = [|mapM (\h@(HBourse x) -> loadInfo x >>= \y -> return (h, y)) $(hxsFromNames s)|]
