{-# LANGUAGE DeriveGeneric #-}

module Authenticated.RetrieveOrders
  ( Order (..)
  , retrieveOrders
  ) where

import Common
import Data.Aeson
import GHC.Generics
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

newtype OrderRaw = OrderRaw [Value] deriving (Eq, Show, Generic)
instance FromJSON OrderRaw

data Order = Order
  { orderID             :: Int
  , orderGID            :: Maybe Int
  , orderCID            :: Int
  , orderSymbol         :: String
  , orderCreateTime     :: UTCTime
  , orderUpateTime      :: UTCTime
  , orderAmount         :: Float
  , orderAmountOriginal :: Float
  , orderType           :: String
  , orderFlags          :: Int
  , orderStatus         :: String
  , orderPrice          :: Float
  , orderAveragePrice   :: Float
  , orderTrailingPrice  :: Float
  , orderAuxLimitPrice  :: Float
  , orderHidden         :: Bool
  , orderPlacedID       :: Int
  , orderRouting        :: String
  } deriving (Eq, Show)

fromRaw :: OrderRaw -> Order
fromRaw (OrderRaw v) = Order oid ogid ocid sym ct ut amt amto typ flgs stat price avgp tp alp hid pid rout
  where decode' a = case fromJSON a of
                      Success a -> a
                      Error e   -> error e
        mstoutc m = posixSecondsToUTCTime $ m / 1000
        oid       = decode' $ head v
        ogid      = decode' $ v!!1
        ocid      = decode' $ v!!2
        sym       = decode' $ v!!3
        ct        = mstoutc $ decode' $ v!!4
        ut        = mstoutc $ decode' $ v!!5
        amt       = decode' $ v!!6
        amto      = decode' $ v!!7
        typ       = decode' $ v!!8
        flgs      = decode' $ v!!12
        stat      = decode' $ v!!13
        price     = decode' $ v!!16
        avgp      = decode' $ v!!17
        tp        = decode' $ v!!18
        alp       = decode' $ v!!19
        hid       = decode' $ v!!23
        pid       = decode' $ v!!24
        rout      = decode' $ v!!28

retrieveOrders :: BitfinexClient -> IO [Order]
retrieveOrders client = map fromRaw <$> queryBitfinexAuthenticated client "r/orders"
