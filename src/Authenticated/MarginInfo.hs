{-# LANGUAGE DeriveGeneric #-}

module Authenticated.MarginInfo
  ( MarginInfo (..)
  , marginInfo
  ) where

import Common
import Data.Aeson
import GHC.Generics

newtype MarginInfoRaw = MarginInfoRaw (String, String, [Float]) deriving (Eq, Show, Generic)
instance FromJSON MarginInfoRaw

data MarginInfo = MarginInfo
  { marginType      :: String
  , marginSymbol    :: String
  , marginTradable  :: Float
  , marginGross     :: Float
  , marginBuy       :: Float
  , marginSell      :: Float
  } deriving (Eq, Show)

marginInfo :: BitfinexClient -> IO MarginInfoRaw
marginInfo client = queryBitfinexAuthenticated client "r/info/margin/tBTCUSD"
