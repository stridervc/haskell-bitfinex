{-# LANGUAGE DeriveGeneric #-}

module Authenticated.Trades
  ( Trade (..)
  , trades
  ) where

import Common
import Data.Aeson
import GHC.Generics

newtype TradeRaw = TradeRaw [Value] deriving (Eq, Show, Generic)
instance FromJSON TradeRaw

data Trade = Trade
  { tradeID           :: Int
  , tradePair         :: String
  , tradeCreate       :: Int
  , tradeOrderID      :: Int
  , tradeAmount       :: Float  -- ^ Positive is buy, negative is sell
  , tradePrice        :: Float
  , tradeOrderType    :: String
  , tradeOrderPrice   :: Float
  , tradeMaker        :: Bool
  , tradeFee          :: Float
  , tradeFeeCurrency  :: String
  } deriving (Eq, Show)

fromRaw :: TradeRaw -> Trade
fromRaw (TradeRaw v) = Trade (d 0) (d 1) (d 2) (d 3) (d 4) (d 5) (d 6) (d 7) maker (d 9) (d 10)
  where decode' a = case fromJSON a of
                      Success a -> a
                      Error e   -> error e
        d i       = decode' $ v!!i
        maker     = (d 8 :: Int) == 1

trades :: BitfinexClient -> String -> IO [Trade]
trades client symbol = map fromRaw <$> queryBitfinexAuthenticated client ("r/trades/" ++ symbol ++ "/hist")
