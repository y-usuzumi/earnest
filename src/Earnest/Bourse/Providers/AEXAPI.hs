module Earnest.Bourse.Providers.AEXAPI where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Crypto.Hash
import           Data.Aeson                       hiding (Options)
import qualified Data.ByteString.Char8            as SBS
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.Default
import           Data.Earnest.Bourse
import           Data.Earnest.Currency
import           Data.Earnest.TradeInfo
import           Data.Hashable                    (Hashable)
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe
import           Data.String.Interpolate.IsString
import           Earnest.Utils
import           GHC.Generics
import           Network.Wreq                     (FormParam ((:=)), defaults,
                                                   header, responseBody,
                                                   responseStatus, statusCode)
import qualified Network.Wreq                     as Req
import           Prelude                          hiding (last)
import           System.Posix.Time
import qualified Text.Read                        as R

-- The programmers that work in AEX are idiots

currencyNameMapping :: HM.HashMap String Currency
currencyNameMapping = HM.fromList [ ("ae", AE)
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
                                  , ("lft", LFT)
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
                                  , ("pax", PAX)
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
                                  , ("tusd", TUSD)
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

apis :: HM.HashMap String String
apis = HM.fromList $ map (second prefixify)
  [ ("balance", "getMyBalance.php")
  , ("ticker_cnc", "ticker.php?c=all&mk_type=cnc")
  , ("ticker_usdt", "ticker.php?c=all&mk_type=usdt")
  , ("ticker_gat", "ticker.php?c=all&mk_type=gat")
  ]
  where
    prefixify = ("https://api.bit.cc/" ++)

postOptions :: Req.Options
postOptions = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"]

data APIFailure = APIFailure Int     -- ^ status code
                             String  -- ^ message

data APISuccess a = APISuccess Int   -- ^ status code
                               a     -- ^ data

type APIResult a = Either APIFailure (APISuccess a)

getParams :: MonadIO m => AEXAPIBourse -> m [Req.FormParam]
getParams AEXAPIBourse{..} = do
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
  return params

accountBalance :: AEXAPIBourse -> IO (APIResult (HM.HashMap String String))
accountBalance b = do
  args <- getParams b
  r <- Req.postWith postOptions (apis HM.! "balance") args
  let t = r ^. Req.responseBody
      s = r ^. Req.responseStatus . Req.statusCode
      respJson = decode t :: Maybe (HM.HashMap String String)
  return $ case respJson of
    Nothing   -> Left (APIFailure s (LBS.unpack t))
    Just json -> Right (APISuccess s json)

data Ticker = Ticker { high :: Double
                     , low  :: Double
                     , last :: Double
                     , vol  :: Double
                     , buy  :: Double
                     , sell :: Double
                     } deriving (Generic)

instance ToJSON Ticker
instance FromJSON Ticker

ticker :: AEXAPIBourse -> String -> IO (APIResult (HM.HashMap String Ticker))
ticker b pricingCoin = do
  r <- Req.get (apis HM.! ("ticker_" ++ pricingCoin))
  let t = r ^. responseBody
      s = r ^. responseStatus . statusCode
      respJson = decode t :: Maybe (HM.HashMap String (HM.HashMap String Ticker))
  return $ case respJson of
    Nothing   -> Left (APIFailure s (LBS.unpack t))
    Just json -> Right (APISuccess s (normalize json))
  where
    normalize = HM.map (\hm -> hm HM.! "ticker")


data AEXAPIBourse = AEXAPIBourse { uid  :: String
                                 , key  :: String
                                 , skey :: String
                                 } deriving (Eq, Generic, Show)

instance Hashable AEXAPIBourse

instance Bourse AEXAPIBourse where
  loadInfo b = do
    accountBalanceResult <- liftIO $ accountBalance b
    balances <- case accountBalanceResult of
      Left (APIFailure _ msg) -> throwM (LoadInfoFailed msg)
      Right (APISuccess _ d)  -> return $ toBalances d
    cncCCTIs <- loadTickerInfo "cnc"
    usdtCCTIs <- loadTickerInfo "usdt"
    gatCCTIs <- loadTickerInfo "gat"
    let tradeInfoTable = flip execState newTradeInfoTable $ do
          merge cncCCTIs
          merge usdtCCTIs
          merge gatCCTIs

    return BourseInfo{ _supportedTrades = tradeInfoTable
                     , _balances = balances
                     , _confidence = 0.2
                     }
    where
      keyCurrMap = HM.fromList $ map (first (++ "_balance")) $ HM.toList currencyNameMapping
      folder newMap k v = case HM.lookup k keyCurrMap of
        Nothing   -> newMap
        Just curr -> case R.readMaybe v :: Maybe Double of
          Just v' -> HM.insert curr v' newMap
          Nothing -> newMap
      toBalances = HM.foldlWithKey' folder HM.empty
      tickerToTI Ticker{..} = def { fee = 0
                                  , buy
                                  , sell
                                  , high
                                  , low
                                  , last
                                  , vol
                                  , depth = def
                                  }
      loadTickerInfo :: ThrowableIO m => String -> m [(Currency, Currency, TradeInfo)]
      loadTickerInfo currStr = do
        tickerResult <- liftIO $ ticker b currStr
        currencies <- case tickerResult of
          Left (APIFailure _ msg) -> throwM (LoadInfoFailed msg)
          Right (APISuccess _ d)  -> return $ HM.map tickerToTI d
        curr <- stc currStr
        cctis <- mapM (\(c, ti) -> stc c >>= return . (, curr, ti)) $ HM.toList currencies
        -- FIXME: Inverse of TradeInfo
        rCCTIs <- mapM (\(c, ti) -> stc c >>= return . (curr, , ti)) $ HM.toList currencies
        return $ cctis ++ rCCTIs
      stc s = case HM.lookup s currencyNameMapping of
        Just curr -> return curr
        Nothing   -> throwM (LoadInfoFailed [i|Invalid currency: #{s}|])
  updateBourseInfo = undefined
