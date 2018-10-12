module Earnest.Bourse.Providers.AEXAPI where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Earnest.Bourse
import           Data.Earnest.Currency
import           Data.Earnest.TradeInfo
import           Data.Hashable              (Hashable)
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe
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

postOptions :: Options
postOptions = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"]

data AEXAPIBourse = AEXAPIBourse { uid  :: String
                                 , key  :: String
                                 , skey :: String
                                 } deriving (Eq, Generic, Show)

instance Hashable AEXAPIBourse

instance Bourse AEXAPIBourse where
  loadInfo b = do
    args <- liftIO $ getArgs b
    r <- liftIO $ postWith postOptions (apis HM.! "balance") args
    let responseText = r ^. responseBody
    let respJson = (decode responseText :: Maybe (HM.HashMap String String))
    liftIO $ print respJson
    when (isNothing respJson) $ liftIO $ throwM (LoadInfoFailed $ LBS.unpack responseText)
    let balances = toBalances $ fromJust respJson
    liftIO $ print balances
    return BourseInfo{ _supportedTrades = newTradeInfoTable
                     , _balances = balances
                     , _confidence = 0.2
                     }
    where
      getArgs AEXAPIBourse{..} = do
        now <- epochTime
        let md5 = hash $ SBS.pack ( key ++ "_"
                                   ++ uid ++ "_"
                                   ++ skey ++ "_"
                                   ++ show now
                                 ) :: MD5
        let params =  [ "key" := key
                      , "time" := show now
                      , "md5" := show md5
                      ]
        liftIO $ print params
        return params
      keyCurrMap = HM.fromList [ ("bcc_balance", BCC)
                               , ("bitcny_balance", BitCNY)
                               , ("cnc_balance", CNC)
                               , ("tac_balance", TAC)
                               , ("fgc_balance", FGC)
                               , ("bash_balance", BASH)
                               , ("xrp_balance", XRP)
                               , ("nss_balance", NSS)
                               , ("tmc_balance", TMC)
                               ]
      toBalances map = HM.foldlWithKey' folder HM.empty map
      folder newMap k v = case HM.lookup k keyCurrMap of
        Nothing   -> newMap
        Just curr -> HM.insert curr (read v :: Double) newMap
