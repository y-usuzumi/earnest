module Data.Earnest.TestData.TH where

import           Data.Earnest.Bourse
import           Language.Haskell.TH


boursesFromNames :: [ExpQ] -> ExpQ
boursesFromNames s = listE $ map (\exp -> [|HBourse $(exp)|]) s
