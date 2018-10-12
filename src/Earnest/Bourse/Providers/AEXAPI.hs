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
apis = HM.fromList $ map (second prefixify)
  [ ("balance", "getMyBalance.php")
  , ("trading", "ticker.php?c=all&mk_type=cnc")
  ]
  where
    prefixify = ("https://api.bit.cc/" ++)

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
    let respJson = decode responseText :: Maybe (HM.HashMap String String)
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
      keyCurrMap = HM.fromList $ map (first (++ "_balance")) [ ("ae", AE)
                                                             , ("ardr", ARDR)
                                                             , ("atn", ATN)
                                                             , ("bash", BASH)
                                                             , ("bcc", BCC)
                                                             , ("bcd", BCD)
                                                             , ("bcv", BCV)
                                                             , ("bcx", BCX)
                                                             , ("bec", BEC)
                                                             , ("bitcny", BitCNY)
                                                             , ("bite", BITE)
                                                             , ("bkbt", BKBT)
                                                             , ("blk", BLK)
                                                             , ("bnt", BNT)
                                                             , ("bost", BOST)
                                                             , ("btc", BTC)
                                                             , ("btm", BTM)
                                                             , ("bts", BTS)
                                                             , ("btsv", BTSV)
                                                             , ("can", CAN)
                                                             , ("cdr", CDR)
                                                             , ("cnc", CNC)
                                                             , ("cnet", CNET)
                                                             , ("cvc", CVC)
                                                             , ("dash", DASH)
                                                             , ("dat", DAT)
                                                             , ("dgc", DGC)
                                                             , ("doge", DOGE)
                                                             , ("eac", EAC)
                                                             , ("ela", ELA)
                                                             , ("emc", EMC)
                                                             , ("eos", EOS)
                                                             , ("eosdac", EOSDAC)
                                                             , ("etc", ETC)
                                                             , ("eth", ETH)
                                                             , ("fgc", FGC)
                                                             , ("gat", GAT)
                                                             , ("gnx", GNX)
                                                             , ("god", GOD)
                                                             , ("hlb", HLB)
                                                             , ("idt", IDT)
                                                             , ("ignis", IGNIS)
                                                             , ("inf", INF)
                                                             , ("jrc", JRC)
                                                             , ("knc", KNC)
                                                             , ("lbtc", LBTC)
                                                             , ("lend", LEND)
                                                             , ("ltc", LTC)
                                                             , ("lxt", LXT)
                                                             , ("mec", MEC)
                                                             , ("mgc", MGC)
                                                             , ("ncs", NCS)
                                                             , ("neo", NEO)
                                                             , ("nss", NSS)
                                                             , ("nuls", NULS)
                                                             , ("nxt", NXT)
                                                             , ("oct", OCT)
                                                             , ("omg", OMG)
                                                             , ("ppc", PPC)
                                                             , ("qash", QASH)
                                                             , ("qrk", QRK)
                                                             , ("ric", RIC)
                                                             , ("sac", SAC)
                                                             , ("sbtc", SBTC)
                                                             , ("seer", SEER)
                                                             , ("snt", SNT)
                                                             , ("stb", STB)
                                                             , ("sys", SYS)
                                                             , ("tac", TAC)
                                                             , ("tag", TAG)
                                                             , ("tmc", TMC)
                                                             , ("tyt", TYT)
                                                             , ("ubtc", UBTC)
                                                             , ("usdt", USDT)
                                                             , ("vash", VASH)
                                                             , ("vns", VNS)
                                                             , ("wdc", WDC)
                                                             , ("wic", WIC)
                                                             , ("xas", XAS)
                                                             , ("xcn", XCN)
                                                             , ("xem", XEM)
                                                             , ("xlm", XLM)
                                                             , ("xpm", XPM)
                                                             , ("xrp", XRP)
                                                             , ("xyt", XYT)
                                                             , ("xzc", XZC)
                                                             , ("ybc", YBC)
                                                             , ("yoyo", YOYO)
                                                             , ("zcc", ZCC)
                                                             ]
      toBalances = HM.foldlWithKey' folder HM.empty
      folder newMap k v = case HM.lookup k keyCurrMap of
        Nothing   -> newMap
        Just curr -> HM.insert curr (read v :: Double) newMap
