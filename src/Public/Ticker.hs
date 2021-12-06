{-# LANGUAGE DeriveGeneric #-}

module Public.Ticker
  ( Ticker (..)
  , ticker
  ) where

import Common

import Data.Aeson
import GHC.Generics

type TickerRaw = [Float]

data Ticker = Ticker
  { tickerBid                 :: Price
  , tickerBidSize             :: Float
  , tickerAsk                 :: Price
  , tickerAskSize             :: Float
  , tickerDailyChange         :: Price
  , tickerDailyChangeRelative :: Percentage
  , tickerLastPrice           :: Price
  , tickerVolume              :: Float
  , tickerHigh                :: Price
  , tickerLow                 :: Price
  } deriving (Eq, Show)

tickerRaw :: BitfinexClient -> String -> IO TickerRaw
tickerRaw client pair = queryBitfinexPublic client $ "ticker" ++ "/" ++ pair

ticker :: BitfinexClient -> String -> IO Ticker
ticker client pair = do
  raw <- tickerRaw client pair
  return $ Ticker
    { tickerBid                 = head raw
    , tickerBidSize             = raw!!1
    , tickerAsk                 = raw!!2
    , tickerAskSize             = raw!!3
    , tickerDailyChange         = raw!!4
    , tickerDailyChangeRelative = raw!!5 * 100
    , tickerLastPrice           = raw!!6
    , tickerVolume              = raw!!7
    , tickerHigh                = raw!!8
    , tickerLow                 = raw!!9
    }
