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
              | BITE
              | BitCNY
              | BKBT
              | BLK
              | BNT
              | BTC
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
              | DOGE
              | EAC
              | ELA
              | EOS
              | EOSDAC
              | ETC
              | ETH
              | FGC
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
              | NSS
              | NULX
              | NXT
              | IGNIS
              | JRC
              | OCT
              | OMG
              | OPC
              | QASH
              | QRK
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
              | WIC
              | XAS
              | XCN
              | XEM
              | XLM
              | XPM
              | XRP
              | XYT
              | XZC
              | YOYO
              | ZCC
              deriving (Enum, Eq, Generic, Ord, Read, Show)

instance Hashable Currency
