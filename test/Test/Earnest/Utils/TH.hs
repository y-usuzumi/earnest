module Test.Earnest.Utils.TH where

import           Earnest.Config
import           Earnest.Utils.TH
import           Language.Haskell.TH

findBourseConfig :: Name -> Name -> ExpQ
findBourseConfig cfgName bourseType =
  [|head $ filter $(isConstructedBy bourseType) $ bourses $(varE cfgName)|]
