module Data.Earnest.Wallet where

import           Data.Typeable

class (Ord w, Show w, Typeable w) => Wallet w where
  type Currency w

data HWallet where
  HWallet :: Wallet w => w -> HWallet

instance Show HWallet where
  showsPrec d (HWallet w) = showParen (d > 10) $ showString "HWallet " . showsPrec 11 w
