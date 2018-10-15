module Test.Earnest.Util where

import           Earnest.Config
import           Language.Haskell.TH

findBourseConfig :: Name -> Name -> ExpQ
findBourseConfig cfgName bourseType =
  [|head $ filter $(f bourseType) $ bourses $(varE cfgName)|]
  where
    calcArity (AppT (AppT ArrowT _) ts) = 1 + calcArity ts
    calcArity _ = 0
    f bourseType = do
      DataConI _ ty _ <- reify bourseType
      [|\case
             $(conP bourseType $ replicate (calcArity ty) wildP) -> True
             _ -> False
       |]
