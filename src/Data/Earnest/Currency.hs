module Data.Earnest.Currency where

import           Data.Hashable
import           GHC.Generics  (Generic)

data Currency = AE
              | ARDR
              | ATN
              | BASH
              | BCC
              | BCD
              | BCV
              | BCX
              | BEC
              | BITE
              | BitCNY
              | BKBT
              | BLK
              | BNT
              | BOST
              | BTC
              | BTSV
              | BTM
              | BTS
              | CAN
              | CDR
              | CNC   -- specific to AEX
              | CNET
              | CNY
              | CVC
              | DASH
              | DAT
              | DGC
              | DOGE
              | EAC
              | ELA
              | EMC
              | EOS
              | EOSDAC
              | ETC
              | ETH
              | FGC
              | GAT
              | GNX
              | GOD
              | HLB
              | IDT
              | INF
              | KNC
              | LTC
              | LXT
              | LBTC
              | LEND
              | LV
              | MEC
              | MGC
              | NCS
              | NEO
              | NSS
              | NULS
              | NXT
              | IGNIS
              | JRC
              | OCT
              | OMG
              | OPC
              | PPC
              | QASH
              | QRK
              | RIC
              | SAC
              | SBTC
              | SEER
              | SNT
              | STB
              | SYS
              | TAC
              | TAG
              | TMC
              | TYT
              | UBTC
              | USD
              | USDT  -- specific to AEX
              | VASH
              | VNS
              | WDC
              | WIC
              | XAS
              | XCN
              | XEM
              | XLM
              | XPM
              | XRP
              | XYT
              | XZC
              | YBC
              | YOYO
              | ZCC
              deriving (Enum, Eq, Generic, Ord, Read, Show)

instance Hashable Currency
