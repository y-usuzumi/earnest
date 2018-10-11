module Earnest.Bourse.Providers.AEXAPI where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson             (decode)
import qualified Data.ByteString.Char8  as BS
import           Data.Earnest.Bourse
import           Data.Earnest.TradeInfo
import           Data.Hashable          (Hashable)
import qualified Data.HashMap.Strict    as HM
import           GHC.Generics
import           Network.Wreq
import           System.Posix.Time

-- The programmers that work in AEX are idiots

apis :: HM.HashMap String String
apis = HM.fromList $ map (id *** prefixify)
  [ ("balance", "getMyBalance.php")
  , ("trading", "ticker.php?c=all&mk_type=cnc")
  ]
  where
    prefixify = ("https://api.aex.com/" ++)

data AEXAPIBourse = AEXAPIBourse { uid  :: String
                                 , key  :: String
                                 , skey :: String
                                 } deriving (Eq, Generic, Show)

instance Hashable AEXAPIBourse

instance Bourse AEXAPIBourse where
  loadInfo b = do
    args <- liftIO $ getArgs b
    r <- liftIO $ post (apis HM.! "balance") args
    let balances = toBalances (decode $ r ^. responseBody :: Maybe (HM.HashMap String String))
    return BourseInfo{ _supportedTrades = newTradeInfoTable
                     , _balances = undefined
                     , _confidence = 0.2
                     }
    where
      getArgs AEXAPIBourse{..} = do
        now <- epochTime
        let md5 = hash $ BS.pack ( key ++ "_"
                                   ++ uid ++ "_"
                                   ++ skey ++ "_"
                                   ++ show now
                                 ) :: MD5
        return [ "key" := key
               , "time" := show now
               , "md5" := show md5
               ]
      toBalances map = undefined  -- TODO
