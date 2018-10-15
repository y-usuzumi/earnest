module Earnest.Utils.TH where

import Language.Haskell.TH

calcArity :: Type -> Int
calcArity (AppT (AppT ArrowT _) ts) = 1 + calcArity ts
calcArity _ = 0


isConstructedBy :: Name -> ExpQ
isConstructedBy con = do
  DataConI _ ty _ <- reify con
  [|\case
      $(conP con $ replicate (calcArity ty) wildP) -> True
      _ -> False
   |]

