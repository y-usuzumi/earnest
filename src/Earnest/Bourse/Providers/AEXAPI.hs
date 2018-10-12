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

data APIFailure = APIFailure Int     -- ^ status code
                             String  -- ^ message

data APISuccess a = APISuccess Int   -- ^ status code
                               a     -- ^ data

type APIResult a = Either APIFailure (APISuccess a)

getArgs :: MonadIO m => AEXAPIBourse -> m [FormParam]
getArgs AEXAPIBourse{..} = do
  now <- liftIO epochTime
  let md5 = hash $ SBS.pack ( key ++ "_"
                              ++ uid ++ "_"
                              ++ skey ++ "_"
                              ++ show now
                            ) :: MD5
      params =  [ "key" := key
                , "time" := show now
                , "md5" := show md5
                ]
  liftIO $ print params
  return params

tradingQuote :: AEXAPIBourse -> IO (APIResult (HM.HashMap String String))
tradingQuote b = do
  args <- getArgs b
  r <- postWith postOptions (apis HM.! "balance") args
  let t = r ^. responseBody
      s = r ^. responseStatus . statusCode
      respJson = decode t :: Maybe (HM.HashMap String String)
  return $ case respJson of
    Nothing -> Left (APIFailure s (LBS.unpack t))
    Just json -> Right (APISuccess s json)

data AEXAPIBourse = AEXAPIBourse { uid  :: String
                                 , key  :: String
                                 , skey :: String
                                 } deriving (Eq, Generic, Show)

instance Hashable AEXAPIBourse

instance Bourse AEXAPIBourse where
  loadInfo b = do
    tqResult <- liftIO $ tradingQuote b
    balances <- case tqResult of
      Left (APIFailure _ msg) -> liftIO $ throwM (LoadInfoFailed msg)
      Right (APISuccess _ d) -> return $ toBalances d
    return BourseInfo{ _supportedTrades = newTradeInfoTable
                     , _balances = balances
                     , _confidence = 0.2
                     }
    where
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
